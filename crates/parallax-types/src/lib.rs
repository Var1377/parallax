//! Type checking and inference for the Parallax programming language.
//!
//! This crate implements type inference, type checking, and trait solving for Parallax.
//! It builds on the name resolution phase (`parallax-resolve`) and provides a fully
//! typed High-level Intermediate Representation (HIR).

// Declare modules
pub mod error;
pub mod types;
pub mod context;
pub mod checker;
mod internal; // Internal implementation details
use crate::error::display_type;

// Re-export key types for external use
pub use error::TypeError;
pub use types::*;
pub use context::{TypeContext, TraitRepository, TraitId, ImplId};

// Import necessary database traits from other crates
use parallax_resolve::{ResolveDatabase, ResolvedModuleStructure};
use parallax_resolve::definitions::DefinitionKind;
use parallax_resolve::types::Symbol;

// --- Main Database Trait & Query ---

/// Database trait integrating type checking operations.
#[salsa::db]
pub trait TypeDatabase: ResolveDatabase {
    /// Performs full type checking on the resolved module structure.
    ///
    /// This is the main entry point for the type checking process. It orchestrates
    /// the different passes (signature collection, body checking, trait solving)
    /// to produce a fully typed module representation (`TypedModule`).
    ///
    /// # Arguments
    /// * `resolved_module`: The output from the name resolution phase, containing
    ///   all resolved definitions and module structure information.
    ///
    /// # Returns
    /// A `TypedModule` containing the type-checked definitions and any errors encountered during the process.
    fn type_check_module(&self, resolved_module: ResolvedModuleStructure) -> TypedModule where Self: Sized {
        // Import checker and other necessary components
        use checker::TypeChecker;
        use context::{TypeContext, TraitRepository};
        use types::{TypedDefinitions, ImplDef, TraitDef, StructDef, EnumDef, TypeDef, FunctionSignature};

        println!("--- Starting Type Checking ---");
    
        // --- Initial Setup ---
        let definitions = resolved_module.definitions(self);
        let type_ctx = TypeContext::new();
        let initial_trait_repo = TraitRepository::new(); // Initial empty repo
        let mut collected_errors = Vec::new(); // Collect errors across passes
    
        // --- Pass 1: Collect Signatures & Impl Stubs --- 
        println!("--- Type Checking Pass 1: Signature Collection & Impl Stubs ---");
        let collected_defs: (Vec<StructDef>, Vec<EnumDef>, Vec<(Symbol, FunctionSignature)>, Vec<TraitDef>, Vec<ImplDef>) = {
            // Create checker for Pass 1 using immutable borrow of initial repo
            let mut checker_pass1 = TypeChecker::new(self, &definitions, &type_ctx, &initial_trait_repo);
            let (collected_structs, collected_enums, collected_functions, collected_traits) = 
                checker::defs::check_definitions_pass1(&mut checker_pass1);
            // Pass the collected traits to check_impl_stubs_pass1
            let impl_defs = checker::impls::check_impl_stubs_pass1(&mut checker_pass1, &collected_traits);
            collected_errors.extend(checker_pass1.errors);
            (collected_structs, collected_enums, collected_functions, collected_traits, impl_defs)
        };
        let (collected_structs, collected_enums, collected_functions, collected_trait_defs, collected_impl_defs) = collected_defs;
        // checker_pass1 goes out of scope here
        println!(
            "--- Pass 1 Complete --- Collected Errors: {}, Structs: {}, Enums: {}, Fns: {}, Traits: {}, Impls: {}", 
            collected_errors.len(), 
            collected_structs.len(),
            collected_enums.len(),
            collected_functions.len(),
            collected_trait_defs.len(), 
            collected_impl_defs.len()
        );
    
        // --- Pass 2: Finalize Impls & Default Symbols ---
        println!("--- Type Checking Pass 2: Finalize Impls & Default Symbols ---");
        let mut final_type_ctx = type_ctx; // Move ownership
        for struct_stub in collected_structs {
            if let Err(e) = final_type_ctx.add_definition(struct_stub.symbol, DefinitionKind::Struct, TypeDef::Struct(struct_stub)) {
                collected_errors.push(e);
            }
        }
        for enum_stub in collected_enums {
            if let Err(e) = final_type_ctx.add_definition(enum_stub.symbol, DefinitionKind::Enum, TypeDef::Enum(enum_stub)) {
                collected_errors.push(e);
            }
        }
        // <<< Add ALL collected function signatures to the final context >>>
        for (func_symbol, func_sig) in collected_functions {
            if let Err(e) = final_type_ctx.add_definition(func_symbol, DefinitionKind::Function, TypeDef::Function(func_sig)) {
                collected_errors.push(e);
            }
        }

        let mut final_trait_repo = initial_trait_repo; // Move ownership
        // Add collected traits first
        for trait_stub in collected_trait_defs {
            if let Err(e) = final_trait_repo.add_trait(
                trait_stub.trait_symbol,
                trait_stub.name,
                trait_stub.generic_params,
                trait_stub.bounds,
                trait_stub.methods,
                trait_stub.associated_types,
                trait_stub.span,
            ) {
                collected_errors.push(e);
            }
        }
        // Add collected impls to the final mutable repo
        for impl_stub in collected_impl_defs {
            if let Err(e) = final_trait_repo.add_impl(
                impl_stub.impl_symbol,
                impl_stub.trait_symbol,
                impl_stub.implementing_type,
                impl_stub.generic_params,
                impl_stub.method_signatures,
                impl_stub.methods,
                impl_stub.default_method_impl_symbols, // Should be empty initially
                impl_stub.associated_type_bindings,
                impl_stub.span,
            ) {
                collected_errors.push(e);
            }
        }
        // Call finalize_impl_defaults
        checker::impls::finalize_impl_defaults(&mut final_trait_repo, &definitions);
        println!("--- Pass 2 Complete --- Errors: {}", collected_errors.len());

        // --- Pass 3: Check Function/Method Bodies ---
        println!("--- Type Checking Pass 3: Body Checking ---");
        let mut final_typed_definitions = TypedDefinitions::default();
        // <<< Store final substitution map after body checking >>>
        let final_subst = {
            // Create checker for Pass 3 using immutable borrow of the *finalized* repo
            // and the *finalized* type_ctx
            let mut checker_pass3 = TypeChecker::new(self, &definitions, &final_type_ctx, &final_trait_repo); 
            // Restore errors collected so far? Or keep separate? Let's keep separate for now.

            // Checks bodies of standalone functions and explicitly defined impl methods.
            checker::defs::check_function_bodies_pass3(&mut checker_pass3, &mut final_typed_definitions);
            checker::impls::check_explicit_impl_bodies_pass3(&mut checker_pass3, &mut final_typed_definitions);
            // <<< Uncomment the call to check default bodies >>>
            checker::impls::check_default_impl_bodies_pass3(&mut checker_pass3, &mut final_typed_definitions);
            
            // <<< Collect errors before solving >>>
            collected_errors.extend(checker_pass3.errors);

            // <<< Run solve to default literal types BEFORE extracting substitution >>>
            match checker_pass3.infctx.solve(&final_trait_repo) {
                 Ok(subst) => subst, // Get final substitution map
                 Err(e) => {
                     collected_errors.push(e);
                     // <<< On error, return the subst *before* solve was attempted >>>
                     // We need a way to get this. Let's clone before solve.
                     // For now, return empty as a fallback. This might hide issues.
                     // TODO: Revisit error handling here. Returning empty might not be ideal.
                     println!("Warning: Solve failed, returning empty substitution.");
                     context::Substitution::new() 
                 }
            }
            // <<< End Solve >>>
        };
        // checker_pass3 goes out of scope here
        println!("--- Pass 3 Complete --- Errors: {}", collected_errors.len());

        // <<< Apply final substitution to all TypedFunction bodies >>>
        println!("--- Applying Final Substitution --- Subst: {}", final_subst); // Print final subst map
        for (symbol, typed_fn) in final_typed_definitions.functions.iter_mut() {
            if let Some(body) = typed_fn.body.as_mut() {
                // <<< DEBUG PRINT BEFORE >>>
                println!(
                    "  [ApplySubst] Before for Fn {:?} ({}) Body Type: {}",
                    symbol,
                    typed_fn.name,
                    display_type(&body.ty)
                );
                // TODO: Maybe add a deep print of the body structure here if needed?

                // Apply substitution recursively throughout the body expression tree
                body.apply_subst_mut(&final_subst);

                // <<< DEBUG PRINT AFTER >>>
                 println!(
                    "  [ApplySubst] After for Fn {:?} ({}) Body Type: {}",
                    symbol,
                    typed_fn.name,
                    display_type(&body.ty)
                );
            } else {
                 println!("  [ApplySubst] Skipping Fn {:?} ({}) - No body", symbol, typed_fn.name);
            }
        }
        println!("--- Final Substitution Complete ---");
        // <<< End Substitution Application >>>
    
        // --- Finalize ---
        // Intrinsics are passed through from the resolved module.
        let intrinsic_symbols = resolved_module.intrinsics(self).to_vec();
    
        TypedModule {
            definitions: final_typed_definitions,
            entry_point: resolved_module.entry_point(self),
            intrinsics: intrinsic_symbols, // Pass through intrinsics from resolver
            errors: collected_errors, // Use combined errors
        }
    }
}
