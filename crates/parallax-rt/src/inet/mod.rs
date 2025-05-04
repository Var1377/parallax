// parallax-rt/src/inet/mod.rs

// Declare the internal modules for the inet runtime
mod manager;
mod partition;
mod readback;
mod reduce;
mod reductions;
mod types;
mod worker;

// Re-export types for external use
pub use manager::RuntimeManager;
pub use partition::Partition;
pub use readback::{readback, ReadbackError, Term};
pub use types::{PartitionIdx, WorkerId}; // Re-export readback items

use crate::error::RuntimeError;
use log;
use num_cpus;
use parallax_gc;
use parallax_net::{
    node::Pointer, port::PortType, Async, CompiledNet, Constructor, Duplicator, Eraser,
    InitialNetConfig, NodeType, Number, Port, Static, Switch, Wire,
};
use parallax_resolve::types::Symbol;
use std::collections::{BTreeMap, HashMap};
use std::sync::{
    atomic::{AtomicU64, Ordering},
    Arc,
};
// Import rsgc thread and args
use parallax_layout::DescriptorStore; // Import DescriptorStore
use rsgc::heap::thread;
use rsgc::prelude::HeapArguments;
use std::error::Error;
use parallax_gc::{set_current_descriptor_store, clear_current_descriptor_store};

// --- Runtime-Specific Definitions --- (Moved from compile_initial_configs)

/// Runtime-internal function identifier (index into CompiledDefs Vec).
pub type FunctionId = usize;

/// Optimized representation of a function's interaction net for lazy loading into partitions.
/// Nodes are stored in Vecs, and ports/redexes use local indices relative to these Vecs.
#[derive(Debug)] // Kept local as it's runtime-specific representation
pub struct FunctionNet {
    pub erasers: Vec<Eraser>,
    pub constructors: Vec<Constructor>,
    pub duplicators: Vec<Duplicator>,
    pub statics: Vec<Static>,
    pub numbers: Vec<Number>,
    pub switches: Vec<Switch>,
    pub asyncs: Vec<Async>,
    pub pointers: Vec<Pointer>,
    /// Initial active pairs within this function body, using *local* indices.
    pub initial_active_pairs: Vec<Wire>,
    /// The root port for this function, using *local* indices.
    pub root: Port,
}

/// Collection of runtime-ready function net definitions, keyed implicitly by FunctionId.
/// Stored in an Arc for cheap sharing between manager and workers.
pub type CompiledDefs = Vec<Arc<FunctionNet>>;

// --- NodeIndexMaps (Used by FunctionNet::from_initial_config and load_function_into_state) ---
#[derive(Debug)]
struct NodeIndexMaps {
    erasers: Vec<usize>,
    constructors: Vec<usize>,
    duplicators: Vec<usize>,
    statics: Vec<usize>,
    numbers: Vec<usize>,
    switches: Vec<usize>,
    asyncs: Vec<usize>,
    pointers: Vec<usize>,
}

impl NodeIndexMaps {
    fn with_capacities(
        e_cap: usize,
        c_cap: usize,
        d_cap: usize,
        r_cap: usize,
        n_cap: usize,
        s_cap: usize,
        a_cap: usize,
        p_cap: usize,
    ) -> Self {
        const PLACEHOLDER: usize = usize::MAX;
        NodeIndexMaps {
            erasers: vec![PLACEHOLDER; e_cap],
            constructors: vec![PLACEHOLDER; c_cap],
            duplicators: vec![PLACEHOLDER; d_cap],
            statics: vec![PLACEHOLDER; r_cap],
            numbers: vec![PLACEHOLDER; n_cap],
            switches: vec![PLACEHOLDER; s_cap],
            asyncs: vec![PLACEHOLDER; a_cap],
            pointers: vec![PLACEHOLDER; p_cap],
        }
    }
    #[inline]
    fn ensure_size(vec: &mut Vec<usize>, old_idx: usize) {
        const PLACEHOLDER: usize = usize::MAX;
        if old_idx >= vec.len() {
            vec.resize(old_idx + 1, PLACEHOLDER);
        }
    }
    #[inline]
    pub fn map_eraser(&mut self, old_idx: usize, new_idx: usize) {
        Self::ensure_size(&mut self.erasers, old_idx);
        self.erasers[old_idx] = new_idx;
    }
    #[inline]
    pub fn map_constructor(&mut self, old_idx: usize, new_idx: usize) {
        Self::ensure_size(&mut self.constructors, old_idx);
        self.constructors[old_idx] = new_idx;
    }
    #[inline]
    pub fn map_duplicator(&mut self, old_idx: usize, new_idx: usize) {
        Self::ensure_size(&mut self.duplicators, old_idx);
        self.duplicators[old_idx] = new_idx;
    }
    #[inline]
    pub fn map_static(&mut self, old_idx: usize, new_idx: usize) {
        Self::ensure_size(&mut self.statics, old_idx);
        self.statics[old_idx] = new_idx;
    }
    #[inline]
    pub fn map_number(&mut self, old_idx: usize, new_idx: usize) {
        Self::ensure_size(&mut self.numbers, old_idx);
        self.numbers[old_idx] = new_idx;
    }
    #[inline]
    pub fn map_switch(&mut self, old_idx: usize, new_idx: usize) {
        Self::ensure_size(&mut self.switches, old_idx);
        self.switches[old_idx] = new_idx;
    }
    #[inline]
    pub fn map_async(&mut self, old_idx: usize, new_idx: usize) {
        Self::ensure_size(&mut self.asyncs, old_idx);
        self.asyncs[old_idx] = new_idx;
    }
    #[inline]
    pub fn map_pointer(&mut self, old_idx: usize, new_idx: usize) {
        Self::ensure_size(&mut self.pointers, old_idx);
        self.pointers[old_idx] = new_idx;
    }
    #[inline]
    pub fn get_eraser(&self, old_idx: usize) -> Option<usize> {
        self.erasers
            .get(old_idx)
            .cloned()
            .filter(|&idx| idx != usize::MAX)
    }
    #[inline]
    pub fn get_constructor(&self, old_idx: usize) -> Option<usize> {
        self.constructors
            .get(old_idx)
            .cloned()
            .filter(|&idx| idx != usize::MAX)
    }
    #[inline]
    pub fn get_duplicator(&self, old_idx: usize) -> Option<usize> {
        self.duplicators
            .get(old_idx)
            .cloned()
            .filter(|&idx| idx != usize::MAX)
    }
    #[inline]
    pub fn get_static(&self, old_idx: usize) -> Option<usize> {
        self.statics
            .get(old_idx)
            .cloned()
            .filter(|&idx| idx != usize::MAX)
    }
    #[inline]
    pub fn get_number(&self, old_idx: usize) -> Option<usize> {
        self.numbers
            .get(old_idx)
            .cloned()
            .filter(|&idx| idx != usize::MAX)
    }
    #[inline]
    pub fn get_switch(&self, old_idx: usize) -> Option<usize> {
        self.switches
            .get(old_idx)
            .cloned()
            .filter(|&idx| idx != usize::MAX)
    }
    #[inline]
    pub fn get_async(&self, old_idx: usize) -> Option<usize> {
        self.asyncs
            .get(old_idx)
            .cloned()
            .filter(|&idx| idx != usize::MAX)
    }
    #[inline]
    pub fn get_pointer(&self, old_idx: usize) -> Option<usize> {
        self.pointers
            .get(old_idx)
            .cloned()
            .filter(|&idx| idx != usize::MAX)
    }
}

// --- Local Port Rewriting Helper (Used by FunctionNet::from_initial_config) ---
fn rewrite_local_port(
    port: Port,
    e_map: &HashMap<usize, usize>,
    c_map: &HashMap<usize, usize>,
    d_map: &HashMap<usize, usize>,
    r_map: &HashMap<usize, usize>,
    n_map: &HashMap<usize, usize>,
    s_map: &HashMap<usize, usize>,
    a_map: &HashMap<usize, usize>,
    p_map: &HashMap<usize, usize>,
) -> Option<Port> {
    if port == Port::NULL {
        return Some(Port::NULL);
    }
    let old_idx = port.node_index() as usize;
    let node_type = NodeType::from_u8(port.node_type())?;
    let port_type = port.port_type();
    if port_type == PortType::Null {
        return None;
    }

    let new_idx = match node_type {
        NodeType::Eraser => *e_map.get(&old_idx)?,
        NodeType::Constructor => *c_map.get(&old_idx)?,
        NodeType::Duplicator => *d_map.get(&old_idx)?,
        NodeType::Static => *r_map.get(&old_idx)?,
        NodeType::Number => *n_map.get(&old_idx)?,
        NodeType::Switch => *s_map.get(&old_idx)?,
        NodeType::Async => *a_map.get(&old_idx)?,
        NodeType::Pointer => *p_map.get(&old_idx)?,
    };
    Some(Port::new(port_type, node_type as u8, 0, new_idx as u64))
}

// --- FunctionNet Conversion (Used by prepare_runtime_defs) ---
impl FunctionNet {
    /// Creates a FunctionNet from an InitialNetConfig.
    /// Performs node conversion and port rewriting to use local Vec indices.
    fn from_initial_config(config: &InitialNetConfig) -> Result<Self, RuntimeError> {
        // Direct conversion from Slab to Vec, preserving order
        let mut erasers = Vec::with_capacity(config.erasers.len());
        let mut e_map = HashMap::with_capacity(config.erasers.len());
        for (slab_idx, node) in &config.erasers {
            let vec_idx = erasers.len();
            erasers.push(node.clone()); // Clone node data
            e_map.insert(slab_idx, vec_idx);
        }
        // ... similar conversion loops for constructors, duplicators, refs, numbers, switches, asyncs ...
        let mut constructors = Vec::with_capacity(config.constructors.len());
        let mut c_map = HashMap::with_capacity(config.constructors.len());
        for (slab_idx, node) in &config.constructors {
            let vec_idx = constructors.len();
            constructors.push(node.clone());
            c_map.insert(slab_idx, vec_idx);
        }
        let mut duplicators = Vec::with_capacity(config.duplicators.len());
        let mut d_map = HashMap::with_capacity(config.duplicators.len());
        for (slab_idx, node) in &config.duplicators {
            let vec_idx = duplicators.len();
            duplicators.push(node.clone());
            d_map.insert(slab_idx, vec_idx);
        }
        let mut statics = Vec::with_capacity(config.statics.len());
        let mut r_map = HashMap::with_capacity(config.statics.len());
        for (slab_idx, node) in config.statics.iter() {
            let vec_idx = statics.len();
            statics.push(Static {
                principle: node.principle,
                data: AtomicU64::new(node.data.load(Ordering::Relaxed)),
            });
            r_map.insert(slab_idx, vec_idx);
        }
        let mut numbers = Vec::with_capacity(config.numbers.len());
        let mut n_map = HashMap::with_capacity(config.numbers.len());
        for (slab_idx, node) in config.numbers.iter() {
            let vec_idx = numbers.len();
            numbers.push(Number {
                principle: node.principle,
                data: node.data,
            });
            n_map.insert(slab_idx, vec_idx);
        }
        let mut switches = Vec::with_capacity(config.switches.len());
        let mut s_map = HashMap::with_capacity(config.switches.len());
        for (slab_idx, node) in &config.switches {
            let vec_idx = switches.len();
            switches.push(node.clone());
            s_map.insert(slab_idx, vec_idx);
        }
        let mut asyncs = Vec::with_capacity(config.asyncs.len());
        let mut a_map = HashMap::with_capacity(config.asyncs.len());
        for (slab_idx, node) in &config.asyncs {
            let vec_idx = asyncs.len();
            asyncs.push(node.clone());
            a_map.insert(slab_idx, vec_idx);
        }
        // Pointers are not part of InitialNetConfig, initialize empty
        let pointers = Vec::new();
        let p_map = HashMap::new();

        // --- First Pass: Rewrite initial active pairs and root port ---
        let initial_active_pairs = config
            .initial_wires
            .iter()
            .filter_map(|wire| {
                let p1 = rewrite_local_port(
                    wire.0, &e_map, &c_map, &d_map, &r_map, &n_map, &s_map, &a_map, &p_map,
                )?;
                let p2 = rewrite_local_port(
                    wire.1, &e_map, &c_map, &d_map, &r_map, &n_map, &s_map, &a_map, &p_map,
                )?;
                if p1 != Port::NULL && p2 != Port::NULL {
                    Some(Wire(p1, p2))
                } else {
                    log::trace!(
                        "Skipping initial wire {:?} during local rewrite due to NULL port.",
                        wire
                    );
                    None
                }
            })
            .collect();

        let root = rewrite_local_port(
            config.root,
            &e_map,
            &c_map,
            &d_map,
            &r_map,
            &n_map,
            &s_map,
            &a_map,
            &p_map,
        )
        .unwrap_or_else(|| {
            log::warn!("Root port became NULL during local rewrite.");
            Port::NULL
        });

        // --- Second Pass: Rewrite internal node ports ---
        for node in constructors.iter_mut() {
            node.left = rewrite_local_port(
                node.left, &e_map, &c_map, &d_map, &r_map, &n_map, &s_map, &a_map, &p_map,
            )
            .unwrap_or(Port::NULL);
            node.right = rewrite_local_port(
                node.right, &e_map, &c_map, &d_map, &r_map, &n_map, &s_map, &a_map, &p_map,
            )
            .unwrap_or(Port::NULL);
        }
        for node in duplicators.iter_mut() {
            node.left = rewrite_local_port(
                node.left, &e_map, &c_map, &d_map, &r_map, &n_map, &s_map, &a_map, &p_map,
            )
            .unwrap_or(Port::NULL);
            node.right = rewrite_local_port(
                node.right, &e_map, &c_map, &d_map, &r_map, &n_map, &s_map, &a_map, &p_map,
            )
            .unwrap_or(Port::NULL);
        }
        for node in switches.iter_mut() {
            node.left = rewrite_local_port(
                node.left, &e_map, &c_map, &d_map, &r_map, &n_map, &s_map, &a_map, &p_map,
            )
            .unwrap_or(Port::NULL);
            node.right = rewrite_local_port(
                node.right, &e_map, &c_map, &d_map, &r_map, &n_map, &s_map, &a_map, &p_map,
            )
            .unwrap_or(Port::NULL);
        }

        Ok(FunctionNet {
            erasers,
            constructors,
            duplicators,
            statics,
            numbers,
            switches,
            asyncs,
            pointers, // Empty for now
            initial_active_pairs,
            root,
        })
    }
}

// --- Global Port Rewriting Helper (Used when loading into partition) ---
fn rewrite_port(port: Port, maps: &NodeIndexMaps, partition_id: u16) -> Port {
    if port == Port::NULL {
        return Port::NULL;
    }
    let old_index = port.node_index() as usize;
    let node_type_u8 = port.node_type();
    let port_type = port.port_type();
    if port_type == PortType::Null {
        return Port::NULL;
    }

    let new_index_opt = match NodeType::from_u8(node_type_u8) {
        Some(NodeType::Eraser) => maps.get_eraser(old_index),
        Some(NodeType::Constructor) => maps.get_constructor(old_index),
        Some(NodeType::Duplicator) => maps.get_duplicator(old_index),
        Some(NodeType::Static) => maps.get_static(old_index),
        Some(NodeType::Number) => maps.get_number(old_index),
        Some(NodeType::Switch) => maps.get_switch(old_index),
        Some(NodeType::Async) => maps.get_async(old_index),
        Some(NodeType::Pointer) => maps.get_pointer(old_index),
        None => {
            log::error!(
                "rewrite_port: Unknown node type {} in port {:?}",
                node_type_u8,
                port
            );
            None
        }
    };

    match new_index_opt {
        Some(new_index) => Port::new(port_type, node_type_u8, partition_id, new_index as u64),
        None => {
            if NodeType::from_u8(node_type_u8) != Some(NodeType::Eraser) {
                log::warn!("rewrite_port: Failed to find new index for old index {} (type {}) in port {:?}. Returning Port::NULL.", old_index, node_type_u8, port);
            }
            Port::NULL
        }
    }
}

// --- Preparing Runtime Definitions ---

/// Converts the CompiledNet artifact into runtime-ready structures.
/// Returns the shared CompiledDefs Vec and the Symbol-to-FunctionId map.
fn prepare_runtime_defs(
    net: &CompiledNet,
) -> Result<(Arc<CompiledDefs>, HashMap<Symbol, FunctionId>), RuntimeError> {
    // Use a BTreeMap to ensure deterministic FunctionId assignment based on Symbol order
    let sorted_networks: BTreeMap<_, _> = net.networks.iter().collect(); // Borrow net
    let num_funcs = sorted_networks.len();

    let mut compiled_defs = Vec::with_capacity(num_funcs);
    let mut symbol_to_id_map = HashMap::with_capacity(num_funcs);
    let mut next_function_id: FunctionId = 0;

    // First pass: Assign FunctionIds (Vec indices) based on sorted order
    for symbol in sorted_networks.keys() {
        symbol_to_id_map.insert(**symbol, next_function_id); // Deref symbol from iterator
        next_function_id += 1;
    }

    // Second pass: Convert InitialNetConfig to FunctionNet and update Static nodes
    for (symbol, initial_config) in sorted_networks {
        // Iterate over sorted BTreeMap
        let function_id = *symbol_to_id_map
            .get(symbol)
            .expect("Symbol must exist in map");
        log::debug!(
            "Preparing runtime net for symbol: {:?} (ID: {})",
            symbol,
            function_id
        );

        // Perform the conversion to the runtime-specific FunctionNet format
        let mut function_net = FunctionNet::from_initial_config(initial_config)?;

        // Update Static nodes holding function pointers.
        let mut statics_updated_count = 0;
        for static_node in function_net.statics.iter_mut() {
            let potential_symbol_id = static_node.data.load(Ordering::Relaxed) as u32;
            let potential_symbol = Symbol::new(potential_symbol_id);

            if let Some(target_function_id) = symbol_to_id_map.get(&potential_symbol) {
                static_node
                    .data
                    .store(*target_function_id as u64, Ordering::Relaxed);
                statics_updated_count += 1;
            }
        }
        if statics_updated_count > 0 {
            log::debug!(
                "Updated {} Static node(s) to store FunctionIds for symbol {:?}",
                statics_updated_count,
                symbol
            );
        }

        // Ensure insertion order matches FunctionId assignment
        if compiled_defs.len() == function_id {
            compiled_defs.push(Arc::new(function_net));
        } else {
            // This should not happen if logic is correct
            log::error!(
                "Function ID mismatch during Vec population. Expected {}, got {}. Symbol: {:?}",
                compiled_defs.len(),
                function_id,
                symbol
            );
            return Err(RuntimeError::Other(
                "Internal error: Function ID mismatch".to_string(),
            ));
        }
    }

    log::info!(
        "Prepared {} runtime function net definitions.",
        compiled_defs.len()
    );
    Ok((Arc::new(compiled_defs), symbol_to_id_map))
}

// --- Dynamic Loading into Partition State (Remains mostly the same) ---

/// Loads the nodes from a FunctionNet into the global PartitionState.
/// Returns the mapping from the FunctionNet's local Vec indices to the global Slab indices.
/// ASSUMES write lock on PartitionState is held by the caller.
fn load_function_into_state(func_net: &FunctionNet, partition: &mut Partition) -> NodeIndexMaps {
    log::trace!(
        "Loading function net into partition {} state...",
        partition.id
    );
    let mut maps = NodeIndexMaps::with_capacities(
        func_net.erasers.len(),
        func_net.constructors.len(),
        func_net.duplicators.len(),
        func_net.statics.len(),
        func_net.numbers.len(),
        func_net.switches.len(),
        func_net.asyncs.len(),
        func_net.pointers.len(),
    );

    for (old_idx, node) in func_net.erasers.iter().enumerate() {
        let new_idx = partition.alloc_eraser(node.clone());
        maps.map_eraser(old_idx, new_idx);
    }
    for (old_idx, node) in func_net.constructors.iter().enumerate() {
        let new_idx = partition.alloc_constructor(node.clone());
        maps.map_constructor(old_idx, new_idx);
    }
    for (old_idx, node) in func_net.duplicators.iter().enumerate() {
        let new_idx = partition.alloc_duplicator(node.clone());
        maps.map_duplicator(old_idx, new_idx);
    }
    for (old_idx, node) in func_net.statics.iter().enumerate() {
        let new_node = Static {
            principle: Port::NULL,
            data: AtomicU64::new(node.data.load(Ordering::Relaxed)),
        };
        let new_idx = partition.alloc_static(new_node);
        maps.map_static(old_idx, new_idx);
    }
    for (old_idx, node) in func_net.numbers.iter().enumerate() {
        let new_node = Number {
            principle: Port::NULL,
            data: node.data,
        };
        let new_idx = partition.alloc_number(new_node);
        maps.map_number(old_idx, new_idx);
    }
    for (old_idx, node) in func_net.switches.iter().enumerate() {
        let new_idx = partition.alloc_switch(node.clone());
        maps.map_switch(old_idx, new_idx);
    }
    for (old_idx, node) in func_net.asyncs.iter().enumerate() {
        let new_idx = partition.alloc_async(node.clone());
        maps.map_async(old_idx, new_idx);
    }
    // Note: Pointers are loaded from func_net, but func_net.pointers is empty
    // Pointers are created dynamically during reduction (e.g., dup<->dup)
    // So the maps.pointers will remain empty here, which is correct.

    log::trace!(
        "Finished loading nodes into partition {} state.",
        partition.id
    );
    maps // Return the maps for rewriting initial active pairs and root
}

// --- Main Runtime Entry Point ---

/// Runs a Parallax program using the partitioned interaction net runtime with lazy loading.
///
/// This function takes the compiled interaction net artifact (`CompiledNet`)
/// and prepares the runtime structures before starting the execution manager.
/// It initializes the GC using `rsgc::thread::main_thread`.
///
/// # Arguments
/// * `net` - The compiled interaction net artifact generated by `parallax-codegen`.
/// * `entry_point_symbol` - The symbol of the function to start execution with.
/// * `entry_type` - The HIR type of the entry point's return value.
/// * `num_workers` - The number of worker threads (defaults to available CPU cores).
///
/// # Returns
/// `Ok(())` on successful execution, or a `RuntimeError`.
pub fn run_inet_partitioned_lazy(
    net: CompiledNet, // Changed from InitialNetConfigCollection
    entry_point_symbol: Symbol,
    entry_type: &parallax_hir::hir::HirType,
    num_workers: Option<usize>,
) -> Result<(), RuntimeError> {
    log::debug!("INET Partitioned Runtime: Preparing GC arguments...");
    let gc_args = HeapArguments::default();
    log::info!("INET Partitioned Runtime: Entering GC main_thread...");

    let gc_thread_result = thread::main_thread(gc_args, move |heap| {
        log::info!("INET Partitioned Runtime: Inside GC main_thread. Heap initialized.");

        // --- Descriptor Store Initialization (similar to native runtime) ---
        log::debug!("INET Partitioned Runtime: Initializing descriptor store...");
        let mut descriptor_store = DescriptorStore {
            descriptors: Vec::new(),
        };
        // SAFETY: Requires GC heap init and mutable store
        unsafe {
            // This is a public function
            parallax_gc::collections::closure::initialize_closure_ref_descriptor(
                &mut descriptor_store,
            );
        }
        let store_box = Box::new(descriptor_store);
        let store_ptr = Box::into_raw(store_box);
        // SAFETY: Requires careful lifetime management.
        unsafe { set_current_descriptor_store(store_ptr); }
        log::info!("INET Partitioned Runtime: Set thread-local descriptor store.");

        // Optional: Register core roots
        // heap.add_core_root_set();

        // --- Prepare Runtime Definitions (inside GC context) ---
        log::info!("Preparing runtime definitions from CompiledNet...");
        // Use try block or map_err for error handling within closure
        let inet_run_result: Result<(), RuntimeError> = (|| {
            let (compiled_defs, symbol_to_id) = prepare_runtime_defs(&net)?;

            // Get FunctionId for the entry point
            let entry_point_id = *symbol_to_id
                .get(&entry_point_symbol)
                .ok_or_else(|| RuntimeError::EntryPointNotFound(entry_point_symbol))?;

            // --- Worker/Partition Setup ---
            let actual_num_workers = num_workers.unwrap_or_else(num_cpus::get);
            log::info!("INET Partitioned Runtime: Using {} worker threads (partitions created dynamically)",
                      actual_num_workers);

            // Create the RuntimeManager
            let mut manager = RuntimeManager::new(
                actual_num_workers,
                Arc::clone(&compiled_defs), // Pass the prepared defs
            );

            // --- Run the Manager ---
            manager.run(entry_point_id, entry_type)?; // Pass entry_point_id and entry_type

            Ok(()) // Return Ok(()) if manager.run succeeded
        })(); // End immediate closure

        // --- Cleanup Descriptor Store ---
        log::debug!("INET Partitioned Runtime: Cleaning up descriptor store...");
        unsafe {
            let _ = Box::from_raw(store_ptr); // Drop the store
            clear_current_descriptor_store();
        }
        log::info!("INET Partitioned Runtime: Cleared thread-local descriptor store.");

        // Map the inner Result<(), RuntimeError> 
        // to the expected Result<(), Box<dyn Error + Send + Sync>>
        match inet_run_result {
            Ok(()) => Ok(()),
            Err(e) => Err(Box::new(e) as Box<dyn Error + Send + Sync>),
        }
    }); // End main_thread closure

    log::info!("INET Partitioned Runtime: Exited GC main_thread.");

    // Process the Result<Result<(), Box<dyn Error + Send + Sync>>, thread::Error>
    match gc_thread_result {
        Ok(()) => Ok(()),
        Err(gc_err) => { // Outer Err case (GC thread error)
            log::error!("INET Partitioned Runtime: GC main_thread error: {:?}", gc_err);
            Err(RuntimeError::Other(format!("GC thread error: {:?}", gc_err)))
        }
    }
}
