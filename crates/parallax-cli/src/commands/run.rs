use crate::{commands::build::handle_build, Backend}; // Reuse build logic
use crate::error::CliError;
use crate::utils::find_frame_root;
use std::path::PathBuf;
use std::process::Command;
use std::env;
// --- New Imports ---
use parallax_db::Compiler;
use parallax_rt::{run_artifact, RuntimeError, run_inet_partitioned_lazy, ExecutionResult};
use parallax_codegen::{CompiledOutput, CodegenError}; // Import CompiledOutput and CodegenError
use parallax_hir::Symbol;
use parallax_hir::hir::{HirType, PrimitiveType};
use parallax_gc::readback::readback_gc; // Import the readback function

// Add error variants for runtime and codegen
impl From<RuntimeError> for CliError {
    fn from(rt_err: RuntimeError) -> Self {
        CliError::RuntimeError(rt_err.to_string())
    }
}
impl From<CodegenError> for CliError {
    fn from(cg_err: CodegenError) -> Self {
        CliError::CodegenError(cg_err.to_string())
    }
}

pub fn handle_run(
    path: Option<PathBuf>,
    release: bool,
    profile: bool,
    backend: Backend,
    args: Vec<String>
) -> Result<(), CliError> {
    // 1. Find the frame root
    let start_path = path.unwrap_or_else(|| env::current_dir().expect("Failed to get current dir"));
    let frame_root = find_frame_root(&start_path)?;
    println!("Running frame at: {} using {:?} backend", frame_root.display(), backend);

    // 2. Compile the project using the database
    println!("Compiling project...");
    let compiler = Compiler::new(frame_root.clone());
    let (compiled_output, entry_symbol, entry_type, hir_module) = compiler.compile_for_run().map_err(CliError::from)?;
    
    // --- Get HIR Defs Needed for Readback --- 
    // Use the returned hir_module directly
    let struct_defs_map: std::collections::HashMap<Symbol, parallax_hir::hir::HirStructDef> = 
        hir_module.structs.into_iter().map(|s| (s.symbol, s)).collect();
    let enum_defs_map: std::collections::HashMap<Symbol, parallax_hir::hir::HirEnumDef> = 
        hir_module.enums.into_iter().map(|e| (e.symbol, e)).collect();
    // --- End HIR Defs --- 
    println!("Compilation successful. Entry point symbol: {:?}, Type: {:?}", entry_symbol, entry_type);

    // 3. Remove initialization call (Runtime is initialized by run functions)
    // println!("Initializing Parallax Runtime...");
    // init_runtime().map_err(CliError::from)?;

    // 4. Select backend and execute
    match backend {
        Backend::Native => {
            println!("Executing using Native backend...");
            let execution_result = run_artifact(compiled_output.native_artifact, entry_symbol, &entry_type, args);

            match execution_result {
                Ok(result) => {
                    match result {
                        ExecutionResult::PrimitiveI8(v) => println!("Program returned (i8): {}", v),
                        ExecutionResult::PrimitiveI16(v) => println!("Program returned (i16): {}", v),
                        ExecutionResult::PrimitiveI32(v) => println!("Program returned (i32): {}", v),
                        ExecutionResult::PrimitiveI64(v) => println!("Program returned (i64): {}", v),
                        ExecutionResult::PrimitiveI128(v) => println!("Program returned (i128): {}", v),
                        ExecutionResult::PrimitiveU8(v) => println!("Program returned (u8): {}", v),
                        ExecutionResult::PrimitiveU16(v) => println!("Program returned (u16): {}", v),
                        ExecutionResult::PrimitiveU32(v) => println!("Program returned (u32): {}", v),
                        ExecutionResult::PrimitiveU64(v) => println!("Program returned (u64): {}", v),
                        ExecutionResult::PrimitiveU128(v) => println!("Program returned (u128): {}", v),
                        ExecutionResult::PrimitiveF32(v) => println!("Program returned (f32): {}", v),
                        ExecutionResult::PrimitiveF64(v) => println!("Program returned (f64): {}", v),
                        ExecutionResult::PrimitiveBool(v) => println!("Program returned (bool): {}", v),
                        ExecutionResult::PrimitiveChar(v) => println!("Program returned (char): '{}'", v),
                        ExecutionResult::Unit => println!("Program returned unit (). Exited successfully."),
                        ExecutionResult::GcHandle(h) => {
                            if h == 0 {
                                println!("Program returned a null GC Handle.");
                            } else {
                                println!("Program returned GC Handle: {:#x}. Attempting readback...", h);
                                match readback_gc(h, &entry_type, &struct_defs_map, &enum_defs_map) {
                                    Ok(term) => {
                                        println!("Readback Result: {:#?}", term);
                                    }
                                    Err(e) => {
                                        println!("GC Readback Error: {}", e);
                                    }
                                }
                            }
                        }
                    }
                    Ok(())
                }
                Err(runtime_error) => {
                    println!("Native Runtime Error: {}", runtime_error);
                    Err(CliError::from(runtime_error))
                }
            }
        }
        Backend::Inet => {
            println!("Executing using INet backend...");
            // Assuming compiled_output has inet_artifact field after DB update
            let execution_result = run_inet_partitioned_lazy(
                compiled_output.inet_artifact,
                entry_symbol,
                &entry_type,
                None
            );

            match execution_result {
                Ok(_) => {
                    println!("INet execution finished.");
                    Ok(())
                }
                Err(runtime_error) => {
                    println!("INet Runtime Error: {}", runtime_error);
                    Err(CliError::from(runtime_error))
                }
            }
        }
        Backend::Hybrid => {
            unimplemented!("Hybrid backend not yet implemented.");
        }
    }
} 