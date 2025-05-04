use crate::NativeError;
use parallax_layout::helpers::get_size_bytes;
use parallax_hir::{HirEnumDef, HirExpr, HirFunction, HirModule, HirStructDef, Symbol};
use cranelift_codegen::settings::{self, Configurable};
use cranelift_codegen::isa::TargetIsa;
use cranelift_module::{Module, Linkage, FuncId, default_libcall_names, DataId};
use cranelift_frontend::FunctionBuilderContext;
use cranelift_jit::{JITModule, JITBuilder};
use std::sync::Arc;
use std::collections::{HashMap, HashSet};
use std::fmt;
use target_lexicon::Triple;
use cranelift_codegen::ir::Function;
use crate::translator::func::*;
use crate::translator::types::*;
use crate::translator::context::{TranslationContext, KnownFunction, GlobalInfo};
use parallax_gc::{self, ClosureRef, GcObject, CLOSURE_REF_DESCRIPTOR_INDEX};
use parallax_layout::{self, LayoutDescriptor, DescriptorStore, DescriptorIndex, LayoutError};
use parallax_hir::hir::{HirType, HirValue, Operand, HirLiteral, AggregateKind, ProjectionKind, PrimitiveType};
use std::mem;
use memoffset::offset_of;
use rsgc::prelude::Handle;

/// Represents a compiled function that can be called from Rust code.
#[derive(Debug)]
pub struct CompiledFunction {
    /// The raw function pointer to the compiled code.
    pub func_ptr: *const u8,
    /// Size of the compiled function in bytes
    pub size: usize,
}

/// Represents the compiled output from the native backend.
pub struct CompiledArtifact {
    /// Map from function symbols to their compiled representation
    pub functions: HashMap<Symbol, CompiledFunction>,
    /// The JIT module that owns the compiled code
    #[allow(dead_code)] // Needs to be kept alive for the function pointers to be valid
    jit_module: Option<JITModule>,
    /// Data IDs and descriptor indices for global static variables that contain GC handles.
    /// The runtime needs this information to register roots with the GC.
    gc_static_roots: Vec<(DataId, DescriptorIndex)>,
}

// Manual Debug implementation since JITModule doesn't implement Debug
impl fmt::Debug for CompiledArtifact {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CompiledArtifact")
            .field("functions", &self.functions)
            .field("jit_module", &"[JITModule]")
            .field("gc_static_roots", &self.gc_static_roots)
            .finish()
    }
}

// Keep track of declared global data during compilation
struct DeclaredGlobal {
    data_id: DataId,
    hir_type: HirType,
    is_mutable: bool,
    // Add symbol if needed for debugging or complex lookups
}

/// Compiles a HIR module into a native artifact (e.g., machine code).
/// Updated signature to accept finalized layout maps.
pub fn compile_hir<'a>(
    hir_module: &'a HirModule,
    descriptor_store: &'a DescriptorStore,
    adt_index_map: &'a HashMap<Symbol, DescriptorIndex>,
    primitive_index_map: &'a HashMap<PrimitiveType, DescriptorIndex>,
    tuple_index_map: &'a HashMap<Vec<HirType>, DescriptorIndex>,
    array_index_map: &'a HashMap<(HirType, usize), DescriptorIndex>,
    handle_descriptor_index: Option<DescriptorIndex>,
) -> Result<CompiledArtifact, NativeError> {
    // --- Setup Cranelift --- //
    let mut flag_builder = settings::builder();
    // Position-independent code is usually desired
    // Using unwrap here since the default flags should always work
    flag_builder.enable("is_pic").unwrap();
    // Enable verifier passes in debug builds
    #[cfg(debug_assertions)]
    flag_builder.set("enable_verifier", "true")?;
    // Disable optimization
    flag_builder.set("opt_level", "none")?;

    let flags = settings::Flags::new(flag_builder);

    // Get the host target triple
    let _target_triple = Triple::host(); // Use the host triple for now
    let isa_builder = cranelift_native::builder()
        .map_err(|e| NativeError::IsaSetupError(format!("Host target lookup failed: {}", e)))?;
    let isa = isa_builder.finish(flags)
        .map_err(|e| NativeError::IsaSetupError(format!("ISA construction failed: {}", e)))?;
    let isa: Arc<dyn TargetIsa> = isa;

    // Create the JIT module
    let builder = JITBuilder::with_isa(isa.clone(), default_libcall_names());
    let mut jit_module = JITModule::new(builder);
    
    // Map to store function symbols -> compiled code
    let mut functions = HashMap::new();

    // --- Track Declared Globals ---
    let mut declared_globals: HashMap<Symbol, DeclaredGlobal> = HashMap::new();

    // --- Prepare HashMaps for Context (struct/enum defs) --- //
    let struct_defs_map: HashMap<Symbol, HirStructDef> = hir_module.structs.iter().map(|s| (s.symbol, s.clone())).collect();
    let enum_defs_map: HashMap<Symbol, HirEnumDef> = hir_module.enums.iter().map(|e| (e.symbol, e.clone())).collect();

    // --- Setup Context (using provided descriptor_store and maps) --- //
    // This context is only used for statics processing before function compilation.
    // We create a *new* context inside translate_function_body with the maps.
    // let mut global_translation_ctx = TranslationContext::new(
    //     &struct_defs_map,
    //     &enum_defs_map,
    //     descriptor_store, 
    //     adt_index_map,
    //     primitive_index_map, // Pass new maps
    //     tuple_index_map,
    //     array_index_map,
    //     None, // TODO: Correctly determine static_closure_ref_descriptor_index
    // );

    // --- Declare and Define Global Statics --- //
    for static_item in &hir_module.statics {
        // TODO: Determine appropriate linkage (e.g., Export if needed externally)
        let linkage = Linkage::Local;
        let data_id = jit_module.declare_data(
            &static_item.name,
            linkage,
            static_item.is_mutable, // Set writability
            false, // tls = false
        )?;

        // --- Define Initializer (Simplified Example: Zero-init) ---
        {
            // Get descriptor index for the static's type
            // Use the provided maps for lookup here.
            let desc_idx = get_descriptor_index_for_type(
                &static_item.ty, 
                adt_index_map, 
                primitive_index_map, 
                tuple_index_map, 
                array_index_map,
                handle_descriptor_index,
            )?;
            // let desc_idx = global_translation_ctx.get_descriptor_index(&static_item.ty)?;
            
            // Get the descriptor from the *provided* store
            let descriptor = descriptor_store.descriptors.get(desc_idx)
                .ok_or_else(|| NativeError::LayoutError(parallax_layout::LayoutError::Other(format!("Descriptor index {} not found for static type {:?}", desc_idx, static_item.ty))))?;
            // Get size using layout helper
            let size_bytes = get_size_bytes(descriptor);

            let mut data_desc = cranelift_module::DataDescription::new();
            // --- Translate Initializer --- //
            let init_value = match &static_item.initializer {
                Some(HirValue::Use(Operand::Const(literal))) => {
                    match literal {
                        // Use the correct variant names and destructure
                        HirLiteral::IntLiteral { value, .. } => Some(cranelift_module::Init::Bytes { contents: value.to_le_bytes().into() }),
                        HirLiteral::FloatLiteral { value, .. } => Some(cranelift_module::Init::Bytes { contents: value.to_le_bytes().into() }), // f64 bits
                        HirLiteral::BoolLiteral(b) => Some(cranelift_module::Init::Bytes { contents: (if *b { 1u8 } else { 0u8 }).to_le_bytes().into() }),
                        HirLiteral::CharLiteral(c) => {
                            let val = *c as u32;
                            Some(cranelift_module::Init::Bytes { contents: val.to_le_bytes().into() })
                        },
                        HirLiteral::StringLiteral(_) => None, // Cannot initialize statics with strings directly yet
                        HirLiteral::Unit => None, // Unit is zero-sized, handled by Zeros below.
                    }
                }
                // TODO: Handle other HirValue initializers if needed (likely requires runtime init)
                _ => None,
            };

            // Use translated value or default to Zeros
            data_desc.init = init_value.unwrap_or(cranelift_module::Init::Zeros { size: size_bytes as usize });
            // --- End Initializer Translation --- //

            jit_module.define_data(data_id, &data_desc)?;
        }
        // --- End Initializer Definition ---

        // Store info for later root registration
        declared_globals.insert(
            static_item.symbol,
            DeclaredGlobal {
                data_id,
                hir_type: static_item.ty.clone(),
                is_mutable: static_item.is_mutable,
            },
        );
    }

    // --- Populate global context with static info (after defining all) --- //
    for (symbol, global_data) in &declared_globals {
        let static_info = hir_module.statics.iter().find(|s| s.symbol == *symbol).unwrap(); // Find original HirGlobalStatic
        // global_translation_ctx.add_global_static_info(*symbol, static_info.name.clone(), global_data.hir_type.clone(), global_data.is_mutable);
    }

    // --- Gather Function Info (including Intrinsics) --- //
    let mut known_functions: HashMap<Symbol, KnownFunction> = HashMap::new();
    // Add regular functions defined in the module
    for func in &hir_module.functions {
        known_functions.insert(func.symbol, KnownFunction {
             name: func.name.clone(),
             return_type: func.signature.return_type.clone(),
             param_types: func.signature.params.iter().map(|(_, ty)| ty.clone()).collect(),
         });
    }
    // Add intrinsic functions (assuming they are also present in hir_module.functions with body=None)
    for (_, intrinsic_symbol) in &hir_module.intrinsics {
        if !known_functions.contains_key(intrinsic_symbol) {
             // Find the HirFunction entry for the intrinsic
             if let Some(func) = hir_module.functions.iter().find(|f| f.symbol == *intrinsic_symbol) {
                 known_functions.insert(func.symbol, KnownFunction {
                     name: func.name.clone(),
                     return_type: func.signature.return_type.clone(),
                     param_types: func.signature.params.iter().map(|(_, ty)| ty.clone()).collect(),
                 });
            } else {
                // This case should ideally not happen if HirModule is consistent
                println!("Warning: Intrinsic symbol {:?} listed in hir_module.intrinsics but not found in hir_module.functions", intrinsic_symbol);
                // Optionally return an error here:
                // return Err(NativeError::CompilationError(format!("Inconsistent HIR: Intrinsic {:?} not found", intrinsic_symbol)));
            }
        }
    }

    // --- Create Intrinsic Symbol Set --- //
    let intrinsic_symbols: HashSet<Symbol> = hir_module.intrinsics.iter().map(|(_, symbol)| *symbol).collect();

    // --- Translate Functions --- //
    
    // PHASE 1: Declare all functions first (including intrinsics)
    // This ensures all function references exist before any definitions are created
    let mut function_ids = HashMap::new();
    for hir_function in &hir_module.functions {
        // Create a temporary context for signature translation
        let mut sig_ctx = TranslationContext::new(
            &struct_defs_map,
            &enum_defs_map,
            descriptor_store,
            adt_index_map,
            primitive_index_map,
            tuple_index_map,
            array_index_map,
            handle_descriptor_index,
            &intrinsic_symbols,
        );
        
        // Declare the function to the JIT module
        let func_id = declare_function(
            &mut jit_module,
            hir_function,
            &isa,
            &mut sig_ctx,
        )?;
        
        // Store the function ID for the definition phase
        function_ids.insert(hir_function.symbol, func_id);
    }
    
    // PHASE 2: Define all functions that have bodies
    for hir_function in &hir_module.functions {
        if let Some(body) = &hir_function.body {
            let func_id = function_ids.get(&hir_function.symbol)
                .ok_or_else(|| NativeError::CompilationError(
                    format!("Function ID for symbol {:?} not found", hir_function.symbol)
                ))?;
                
            let mut function_builder_ctx = FunctionBuilderContext::new();
            let mut ctx = jit_module.make_context();

            // Translate the function body
            let translated_func = translate_function_body(
                hir_module,
                hir_function,
                body, 
                *func_id,
                &mut function_builder_ctx,
                &mut jit_module,
                &isa,
                &known_functions,
                &struct_defs_map, 
                &enum_defs_map,   
                descriptor_store, 
                adt_index_map,    
                primitive_index_map,
                tuple_index_map,
                array_index_map,
                handle_descriptor_index,
                &intrinsic_symbols,
            )?;

            ctx.func = translated_func;
            jit_module.define_function(*func_id, &mut ctx)?;
        }
        // Note: Functions without bodies (like intrinsics) are just declared but not defined
    }

    // --- Finalize Definitions --- //
    jit_module.finalize_definitions()?;
    
    // --- Register Global Roots --- //
    // (Uses HashMaps created earlier)
    for (_symbol, global_info) in &declared_globals {
        // Check if the type might contain a handle *and* if it's mutable
        let mut visiting = HashSet::new();
        // Use the function from parallax_gc and pass the HashMaps
        if global_info.is_mutable && parallax_gc::hir_type_contains_gc_handle(
            &global_info.hir_type,
            &struct_defs_map,
            &enum_defs_map,
            &mut visiting
        ) {
            let (data_ptr_const, _size) = jit_module.get_finalized_data(global_info.data_id);
            let data_ptr_mut = data_ptr_const as *mut u8;
            // SAFETY: We've finalized. The pointer is valid. We trust the type check.
            unsafe {
                // Call the function in the parallax_gc crate
                parallax_gc::register_global_root(data_ptr_mut);
            }
        }
    }

    // --- Collect Compiled Functions (as before) --- //
    for hir_function in &hir_module.functions {
        if hir_function.body.is_some() {
            // Look up the function ID that we previously declared
            // We need to get the FuncId of the function before we can get its pointer
            // Use the declared name to find the function ID
            if let Some(func_or_data_id) = jit_module.get_name(&hir_function.name) {
                if let cranelift_module::FuncOrDataId::Func(func_id) = func_or_data_id {
                    // Get a pointer to the compiled function using the FuncId
                    let func_ptr = jit_module.get_finalized_function(func_id);
                    
                    // Create a CompiledFunction struct with the pointer
                    let compiled_func = CompiledFunction {
                        func_ptr: func_ptr as *const u8, 
                        size: 0, // We don't track size yet, but could estimate it
                    };
                    
                    // Add it to our map
                    functions.insert(hir_function.symbol, compiled_func);
                } else {
                     // Handle case where name refers to data, though unlikely here
                     return Err(NativeError::Unimplemented(format!(
                        "Expected function symbol '{}' to refer to a function, but found data.",
                        hir_function.name
                    )));
                }
            } else {
                 // Function name not found in the module after definition
                 return Err(NativeError::Unimplemented(format!(
                    "Function '{}' declared but not found in finalized module.",
                    hir_function.name
                )));
            }
        }
    }

    Ok(CompiledArtifact {
        functions,
        jit_module: Some(jit_module),
        gc_static_roots: Vec::new(),
    })
}

/// Declares a function to the module using its translated signature.
fn declare_function<'a>(
    jit_module: &mut JITModule,
    hir_function: &HirFunction,
    isa: &Arc<dyn TargetIsa>,
    translation_ctx: &mut TranslationContext<'a>, // Context needs lifetime from store/map
) -> Result<FuncId, NativeError> {
    // Translate the HIR signature using context
    let sig = translate_signature(
        &hir_function.signature,
        isa,
        translation_ctx, // Pass context
    )?;

    let func_id = jit_module.declare_function(
        &hir_function.name,
        Linkage::Export, // Keep as Export?
        &sig,
    )?;

    Ok(func_id)
}

/// Helper function to look up descriptor index using the provided maps.
/// This is used during static initialization before the main TranslationContext is built.
fn get_descriptor_index_for_type(
    hir_type: &HirType,
    adt_map: &HashMap<Symbol, DescriptorIndex>,
    primitive_map: &HashMap<PrimitiveType, DescriptorIndex>,
    tuple_map: &HashMap<Vec<HirType>, DescriptorIndex>,
    array_map: &HashMap<(HirType, usize), DescriptorIndex>,
    handle_index: Option<DescriptorIndex>,
) -> Result<DescriptorIndex, NativeError> {
    match hir_type {
        HirType::Primitive(p) => primitive_map.get(p)
            .copied()
            .ok_or_else(|| NativeError::LayoutError(LayoutError::Other(format!("Missing primitive descriptor index for {:?}", p)))),
        HirType::Adt(s) => adt_map.get(s)
            .copied()
            .ok_or_else(|| NativeError::LayoutError(LayoutError::UnknownAdt(*s))),
        HirType::Tuple(elems) => tuple_map.get(elems)
            .copied()
            .ok_or_else(|| NativeError::LayoutError(LayoutError::Other(format!("Missing tuple descriptor index for {:?}", elems)))),
        HirType::Array(elem_ty, size_opt) => match size_opt {
            Some(size) => {
                let key = (elem_ty.as_ref().clone(), *size);
                array_map.get(&key)
                    .copied()
                    .ok_or_else(|| NativeError::LayoutError(LayoutError::Other(format!("Missing array descriptor index for {:?}", key))))
            }
            None => {
                handle_index.ok_or_else(|| NativeError::LayoutError(LayoutError::Other("Handle descriptor index not set for dynamic Array type".to_string())))
            }
        }
        HirType::FunctionPointer(..) => handle_index
            .ok_or_else(|| NativeError::LayoutError(LayoutError::Other("Handle descriptor index not set for FunctionPointer".to_string()))),
        HirType::Never => primitive_map.get(&PrimitiveType::Unit)
            .copied()
            .ok_or_else(|| NativeError::LayoutError(LayoutError::Other("Missing Unit descriptor index for Never type".to_string()))),
    }
}

impl CompiledArtifact {
    /// Get a function pointer for a specified symbol
    pub fn get_function_ptr(&self, symbol: Symbol) -> Option<*const u8> {
        self.functions.get(&symbol).map(|f| f.func_ptr)
    }
    
    /// Cast a function pointer to a callable Rust function.
    /// 
    /// # Safety
    ///
    /// This is unsafe because the caller must ensure:
    /// 1. The function signature matches what was compiled
    /// 2. The function is correctly implemented and won't cause undefined behavior
    /// 3. The JIT module is still alive (contained in self)
    pub unsafe fn get_function<F>(&self, symbol: Symbol) -> Option<F> 
    where
        F: Copy, // Function pointers are Copy
    {
        self.get_function_ptr(symbol)
            .map(|f_ptr| std::mem::transmute_copy(&f_ptr))
    }
    
    /// Helper to call a nullary (no arguments) function that returns an i64
    /// 
    /// # Safety
    ///
    /// Caller must ensure the function was compiled with a matching signature
    pub unsafe fn call_nullary_i64(&self, symbol: Symbol) -> Option<i64> {
        let f = self.get_function::<fn() -> i64>(symbol)?;
        Some(f())
    }
}