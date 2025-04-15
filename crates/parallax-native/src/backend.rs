use crate::NativeError;
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
use crate::translator::context::{TranslationContext, KnownFunction};
use crate::translator::layout::{LayoutComputer, new_layout_computer, get_layout, get_size_bytes};
use parallax_gc;
use parallax_hir::hir::{HirType, ResolvePrimitiveType, HirValue, Operand, HirLiteral};

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
}

// Manual Debug implementation since JITModule doesn't implement Debug
impl fmt::Debug for CompiledArtifact {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CompiledArtifact")
            .field("functions", &self.functions)
            .field("jit_module", &"[JITModule]")
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
    // Enable optimization
    flag_builder.set("opt_level", "speed")?;

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
    let mut function_builder_ctx = FunctionBuilderContext::new();

    // --- Track Declared Globals ---
    let mut declared_globals: HashMap<Symbol, DeclaredGlobal> = HashMap::new();

    // --- Setup Context and Layout Computer (for type checking roots/globals) ---
    let mut global_translation_ctx = TranslationContext::new(&hir_module.structs, &hir_module.enums);
    // Known functions aren't added here, as type checking doesn't need them

    // --- Declare and Define Global Statics --- 
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
        // WARNING: Requires layout calculation for correct size!
        {
            let mut layout_computer = new_layout_computer(); 
            let layout = get_layout(&static_item.ty, &mut layout_computer, &global_translation_ctx)?;
            let size_bytes = get_size_bytes(&layout);

            let mut data_desc = cranelift_module::DataDescription::new();
            // --- Translate Initializer ---
            let init_value = match &static_item.initializer {
                Some(HirValue::Use(Operand::Const(literal))) => {
                    match literal {
                        HirLiteral::Int(i) => Some(cranelift_module::Init::Bytes { contents: i.to_le_bytes().into() }),
                        HirLiteral::Float(f) => Some(cranelift_module::Init::Bytes { contents: f.to_le_bytes().into() }), // f64 bits
                        HirLiteral::Bool(b) => Some(cranelift_module::Init::Bytes { contents: (if *b { 1u8 } else { 0u8 }).to_le_bytes().into() }), // Dereference b
                        HirLiteral::Char(c) => {
                            // Dereference the matched reference `c`
                            let val = *c as u32; 
                            Some(cranelift_module::Init::Bytes { contents: val.to_le_bytes().into() })
                        },
                        // String literals need Init::Symbol { name: ..., offset: ... } or similar - TBD
                        // Unit is zero-sized, handled by Zeros below.
                        _ => None, 
                    }
                }
                // TODO: Handle other HirValue initializers if needed (likely requires runtime init)
                _ => None, 
            };

            // Use translated value or default to Zeros
            data_desc.init = init_value.unwrap_or(cranelift_module::Init::Zeros { size: size_bytes as usize }); 
            // --- End Initializer Translation ---
            
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

    // --- Populate global context with static info (after defining all) ---
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
            // Create context and layout computer for *this function's* scope
            let mut func_translation_ctx = TranslationContext::new(&hir_module.structs, &hir_module.enums);
            let mut func_layout_computer = new_layout_computer();

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
                &func_translation_ctx, // Pass function-specific context
                &mut func_layout_computer,
            )?;

            // Prepare context for body translation
            let mut ctx = jit_module.make_context();
            ctx.func = Function::new();
            ctx.func.signature = translate_signature(
                 &hir_function.signature,
                 &isa,
                 &func_translation_ctx, // Use function context
                 &mut func_layout_computer
            )?;

            // Translate the function body
            translate_function_body(
                hir_module,
                hir_function,
                body,
                &mut function_builder_ctx,
                &mut ctx,
                &mut jit_module,
                &isa,
                &known_functions, // Pass known functions map
                &hir_module.structs,
                &hir_module.enums,
            )?;

            // Define the function
            jit_module.define_function(func_id, &mut ctx)?;
            jit_module.clear_context(&mut ctx);
        }
        // TODO: Handle extern functions (declare only)
    }

    // --- Finalize Definitions --- //
    jit_module.finalize_definitions()?;
    
    // --- Register Global Roots ---
    // Pre-compute HashMaps for struct/enum definitions
    let struct_defs_map: HashMap<Symbol, HirStructDef> = hir_module.structs.iter().map(|s| (s.symbol, s.clone())).collect();
    let enum_defs_map: HashMap<Symbol, HirEnumDef> = hir_module.enums.iter().map(|e| (e.symbol, e.clone())).collect();

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
    })
}

/// Declares a function to the module using its translated signature.
fn declare_function(
    jit_module: &mut JITModule,
    hir_function: &HirFunction,
    isa: &Arc<dyn TargetIsa>,
    translation_ctx: &TranslationContext,
    layout_computer: &mut LayoutComputer,
) -> Result<FuncId, NativeError> {
    // Translate the HIR signature using context and layout computer
    let sig = translate_signature(
        &hir_function.signature,
        isa,
        translation_ctx,
        layout_computer,
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

pub struct NativeBackend {
    /// The Cranelift JIT module, which manages the JIT'd code.
    jit_module: JITModule,
    /// The target ISA (Instruction Set Architecture).
    isa: Arc<dyn TargetIsa>,
}

impl NativeBackend {
    /// Create a new NativeBackend for the host machine.
    pub fn new() -> Result<Self, NativeError> {
        let mut flag_builder = settings::builder();
        // Enable verifier passes in debug builds
        #[cfg(debug_assertions)]
        flag_builder.set("enable_verifier", "true")?;
        
        // Enable optimization
        flag_builder.set("opt_level", "speed")?;
        
        let flags = settings::Flags::new(flag_builder);
        let isa_builder = cranelift_native::builder()
             .map_err(|e| NativeError::IsaSetupError(format!("Host target lookup failed: {}", e)))?;
        
        let isa = isa_builder
            .finish(flags)
            .map_err(|e| NativeError::IsaSetupError(format!("ISA construction failed: {}", e)))?;
        
        let builder = JITBuilder::with_isa(isa.clone(), cranelift_module::default_libcall_names());
        
        let jit_module = JITModule::new(builder);
        
        Ok(Self { jit_module, isa })
    }
    
    /// Compile a HIR function into native code.
    pub fn compile_function(&mut self, hir_module: &HirModule, func: &HirFunction, body: &HirExpr) -> Result<*const u8, NativeError> {
        // Create context and layout computer
        let mut translation_ctx = TranslationContext::new(&hir_module.structs, &hir_module.enums);
        let mut layout_computer = new_layout_computer();
        // TODO: Populate context with known functions if needed by translate_signature/body
        for (symbol, info) in hir_module.functions.iter().map(|f| (f.symbol, KnownFunction {
            name: f.name.clone(),
            return_type: f.signature.return_type.clone(),
            param_types: f.signature.params.iter().map(|(_, ty)| ty.clone()).collect(),
        })) {
            translation_ctx.add_function_info(symbol, info);
        }

        // Create the known_functions map needed by translate_function_body
        let known_functions: HashMap<Symbol, KnownFunction> = hir_module.functions.iter().map(|f| (f.symbol, KnownFunction {
            name: f.name.clone(),
            return_type: f.signature.return_type.clone(),
            param_types: f.signature.params.iter().map(|(_, ty)| ty.clone()).collect(),
        })).collect();

        // Translate the HIR function signature using context/layout
        let signature = translate_signature(
            &func.signature,
            &self.isa,
            &translation_ctx,
            &mut layout_computer,
        )?;

        let id = self.jit_module
            .declare_function(&func.name, cranelift_module::Linkage::Export, &signature)?;

        let mut ctx = self.jit_module.make_context();
        ctx.func = Function::new();
        ctx.func.signature = signature;

        let mut func_builder_ctx = FunctionBuilderContext::new();

        // Translate the function body using context/layout
        translate_function_body(
            hir_module,
            func,
            body,
            &mut func_builder_ctx,
            &mut ctx,
            &mut self.jit_module,
            &self.isa,
            &known_functions,
            &hir_module.structs,
            &hir_module.enums,
        )?;

        self.jit_module.define_function(id, &mut ctx)?;
        self.jit_module.finalize_definitions()?;
        let code = self.jit_module.get_finalized_function(id);

        Ok(code)
    }
}

// --- Helper function to check for potential GC handles ---

// Removed: hir_type_contains_gc_handle function is now in parallax_gc crate
/*
/// Recursively checks if a HIR type could potentially contain a GC Handle.
fn hir_type_contains_gc_handle(
    hir_type: &HirType,
    ctx: &TranslationContext, // Needed for ADT definitions
    visiting: &mut HashSet<Symbol>, // For cycle detection
) -> bool { ... function body ... } 
*/ 