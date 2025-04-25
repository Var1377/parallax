use crate::NativeError;
use parallax_gc::layout::helpers::get_size_bytes;
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
use parallax_gc::{self, ClosureRef, DescriptorIndex, DescriptorStore, GcObject, LayoutDescriptor, CLOSURE_REF_DESCRIPTOR_INDEX};
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
    /// Storage for TypeDescriptors generated during compilation.
    /// This needs to be kept alive for the GC to use the descriptors.
    descriptor_store: DescriptorStore,
}

// Manual Debug implementation since JITModule doesn't implement Debug
impl fmt::Debug for CompiledArtifact {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CompiledArtifact")
            .field("functions", &self.functions)
            .field("jit_module", &"[JITModule]")
            .field("descriptor_store", &self.descriptor_store) // Debug the store
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
pub fn compile_hir(hir_module: &HirModule) -> Result<CompiledArtifact, NativeError> {
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

    // --- State for Compilation --- //
    let mut descriptor_store = DescriptorStore { descriptors: Vec::new() };
    // Map from ADT Symbol -> DescriptorIndex (still needed during compilation)
    let mut adt_descriptor_indices: HashMap<Symbol, DescriptorIndex> = HashMap::new();
    // Index of the static ClosureRef descriptor (needs to be initialized)
    let mut static_closure_ref_descriptor_index: Option<DescriptorIndex> = None;

    // --- Initialize Standard Descriptors --- //
    // Placeholder for index 0: ClosureRef (Struct)
    descriptor_store.descriptors.push(LayoutDescriptor::Struct { 
        size_bytes: mem::size_of::<ClosureRef>(), 
        align_bytes: mem::align_of::<ClosureRef>(), 
        fields: Box::new([(offset_of!(ClosureRef, env_handle), 2)]), // Assumes Handle is index 2
        // --- Add handle_offsets for env_handle --- 
        handle_offsets: Box::new([offset_of!(ClosureRef, env_handle)]),
    });
    
    // Use descriptor index 0 for ClosureRef
    static_closure_ref_descriptor_index = Some(0);
    
    // Ensure this matches what parallax-gc expects (need to access it safely)
    unsafe {
        if CLOSURE_REF_DESCRIPTOR_INDEX.is_none() {
            println!("Warning: CLOSURE_REF_DESCRIPTOR_INDEX in parallax-gc not initialized!");
        } else if CLOSURE_REF_DESCRIPTOR_INDEX.unwrap() != 0 {
            println!("Warning: ClosureRef descriptor index mismatch between native backend and GC!");
        }
    }

    // Placeholder for index 1: Primitive (Pointer size) - For func_ptr in ClosureRef etc.
    let pointer_size = mem::size_of::<*const u8>();
    let pointer_align = mem::align_of::<*const u8>();
    descriptor_store.descriptors.push(LayoutDescriptor::Primitive { size_bytes: pointer_size, align_bytes: pointer_align });

    // Placeholder for index 2: Handle
    descriptor_store.descriptors.push(LayoutDescriptor::Handle);

    // --- Prepare HashMaps for Context --- //
    let struct_defs_map: HashMap<Symbol, HirStructDef> = hir_module.structs.iter().map(|s| (s.symbol, s.clone())).collect();
    let enum_defs_map: HashMap<Symbol, HirEnumDef> = hir_module.enums.iter().map(|e| (e.symbol, e.clone())).collect();

    // --- Setup Context (using HashMaps and mutable descriptor state) --- //
    // No global context needed just for statics definition?
    // Let's create one anyway for consistency and potential future use.
    let mut global_translation_ctx = TranslationContext::new(
        &struct_defs_map,
        &enum_defs_map,
        &mut descriptor_store.descriptors, // Pass mutable slice/ref
        &mut adt_descriptor_indices,
        static_closure_ref_descriptor_index,
    );

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
            // *** TODO: Implement proper size calculation using LayoutDescriptor ***
            // Get descriptor index for the static's type
            let desc_idx = global_translation_ctx.get_or_create_descriptor_index(&static_item.ty)?;
            // Get the descriptor from the store
            let descriptor = global_translation_ctx.get_descriptor_by_index(desc_idx)
                .ok_or_else(|| NativeError::LayoutError(parallax_gc::layout::LayoutError::Other(format!("Descriptor index {} not found for static type {:?}", desc_idx, static_item.ty))))?;
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
        global_translation_ctx.add_global_static_info(*symbol, static_info.name.clone(), global_data.hir_type.clone(), global_data.is_mutable);
    }

    // --- Gather Function Info (as before) --- //
    let mut known_functions: HashMap<Symbol, KnownFunction> = HashMap::new();
    for func in &hir_module.functions {
        known_functions.insert(func.symbol, KnownFunction {
             name: func.name.clone(),
             return_type: func.signature.return_type.clone(),
             param_types: func.signature.params.iter().map(|(_, ty)| ty.clone()).collect(),
         });
    }

    // --- Translate Functions --- //
    for hir_function in &hir_module.functions {
        if let Some(body) = &hir_function.body {
            // Create a new builder context for each function to avoid state issues
            let mut function_builder_ctx = FunctionBuilderContext::new();

            // Create context and layout computer for *this function's* scope
            // Pass mutable references to the central descriptor state
            let mut func_translation_ctx = TranslationContext::new(
                &struct_defs_map,
                &enum_defs_map,
                &mut descriptor_store.descriptors,
                &mut adt_descriptor_indices,
                static_closure_ref_descriptor_index,
            );

            // Add known functions to context
            for (symbol, info) in &known_functions {
                func_translation_ctx.add_function_info(*symbol, info.clone());
            }
            // Add global static info to func_translation_ctx for access within functions
            for (symbol, global_data) in &declared_globals {
                 let static_info = hir_module.statics.iter().find(|s| s.symbol == *symbol).unwrap(); // Find original HirGlobalStatic
                 func_translation_ctx.add_global_static_info(*symbol, static_info.name.clone(), global_data.hir_type.clone(), global_data.is_mutable);
            }

            // Declare the function
            let func_id = declare_function(
                &mut jit_module,
                hir_function,
                &isa,
                &mut func_translation_ctx, // Pass mutable context
            )?;

            // Prepare context for body translation
            let mut ctx = jit_module.make_context();

            // Translate the function body
            // *** Pass descriptor state down ***
            let translated_func = translate_function_body(
                hir_module,
                hir_function,
                body, // Pass the body expression
                func_id,
                &mut function_builder_ctx,
                &mut jit_module,
                &isa,
                &known_functions,
                &struct_defs_map, // Pass map directly
                &enum_defs_map,   // Pass map directly
                // Pass mut refs to descriptor state
                &mut descriptor_store.descriptors,
                &mut adt_descriptor_indices,
                static_closure_ref_descriptor_index,
            )?;

            // Assign the translated function to the context before defining
            ctx.func = translated_func;

            // Define the function
            jit_module.define_function(func_id, &mut ctx)?;
        }
        // TODO: Handle extern functions (declare only)
    }

    // --- Finalize Definitions --- //
    jit_module.finalize_definitions()?;
    
    // --- Set Global Descriptor Store Pointer for GC --- //
    // SAFETY: We assume compilation is done and the store is stable.
    // The store must outlive the JIT module and any GC activity.
    unsafe {
        parallax_gc::set_descriptor_store(&descriptor_store as *const _);
    }

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
        descriptor_store,
    })
}

/// Declares a function to the module using its translated signature.
fn declare_function(
    jit_module: &mut JITModule,
    hir_function: &HirFunction,
    isa: &Arc<dyn TargetIsa>,
    translation_ctx: &mut TranslationContext, // Pass context mutably
) -> Result<FuncId, NativeError> {
    // Translate the HIR signature using context
    let sig = translate_signature(
        &hir_function.signature,
        isa,
        translation_ctx, // Pass mutably
    )?;

    let func_id = jit_module.declare_function(
        &hir_function.name,
        Linkage::Export,
        &sig,
    )?;

    Ok(func_id)
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