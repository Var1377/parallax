use std::path::PathBuf;
use parallax_source::SourceDatabase;
use parallax_syntax::SyntaxDatabase;
use parallax_resolve::ResolveDatabase;
use parallax_types::TypeDatabase;
use parallax_hir::lower::lower_module_to_anf_hir;
use parallax_hir::{perform_dce, HirModule, Symbol};
use parallax_hir::hir::HirType;
use parallax_codegen::{self, CompiledOutput};
use salsa::Database;

use crate::error::{DatabaseResult, DatabaseError};

/// The central database for the Parallax compiler that integrates all query groups.
#[salsa::db]
#[derive(Default, Clone)]
pub struct Compiler {
    storage: salsa::Storage<Self>,
    root_config: PathBuf,
}

#[salsa::db]
impl salsa::Database for Compiler {
    fn salsa_event(&self, event: &dyn Fn() -> salsa::Event) {
        let event = event();
        println!("Event: {event:?}");
    }
}

macro_rules! impl_source_database {
    ($($db:ident),*) => {
        $(
            #[salsa::db]
            impl $db for Compiler {}
        )*
    };
}

impl_source_database!(SourceDatabase, SyntaxDatabase, ResolveDatabase, TypeDatabase);

impl Compiler {
    pub fn new(crate_root: PathBuf) -> Self {
        Compiler {
            storage: Default::default(),
            root_config: crate_root,
        }
    }

    /// Compile the project to HIR.
    /// This is useful for checks or intermediate steps.
    pub fn compile_to_hir<'db>(&'db self) -> DatabaseResult<HirModule> {
        // Resolve the root frame
        let root_frame = self.load_frame(self.root_config.to_str().unwrap_or("."));
        let module_tree = self.parse_frame(root_frame);
        let resolved_module_tree = self.resolve_definitions(module_tree);

        self.attach(|_| {
            println!("Resolved module tree: {:#?}", resolved_module_tree);
        });

        if resolved_module_tree.errors(self).is_empty() {
            println!("Resolution successful.");
        } else {
            for error in resolved_module_tree.errors(self) {
                return Err(error.clone().into());
            }
        }

        self.attach(|_| {
            // println!("Resolved module tree: {:#?}", resolved_module_tree);
        });

        let typed_module_tree = self.type_check_module(resolved_module_tree);

        self.attach(|_| {
            // println!("Typed module tree: {:#?}", typed_module_tree);
        });

        if typed_module_tree.errors.is_empty() {
            println!("Type checking successful.");
        } else {
            panic!("Type checking errors: {:#?}", typed_module_tree.errors);
        }
        
        println!("Lowering to HIR...");
        let hir = lower_module_to_anf_hir(&typed_module_tree);

        // println!("HIR: {:#?}", hir);

        
        let dce_hir = perform_dce(hir);
        
        println!("DCE'ed HIR: {:?}", dce_hir.functions);

        Ok(dce_hir)
    }

    /// Compile the project for running, producing the combined compiled output
    /// (native and INet artifacts), entry point symbol, the entry point's
    /// HIR type signature, and the final HIR module used for codegen.
    ///
    /// This function performs the full compilation pipeline including code generation
    /// for all supported backends.
    pub fn compile_for_run<'db>(
        &'db self
    ) -> DatabaseResult<(CompiledOutput, Symbol, HirType, HirModule)> {
        let hir_module = self.compile_to_hir()?;

        println!("Searching for main function...");
        let main_fn = hir_module.functions.iter()
            .find(|f| f.name == "main")
            .ok_or(DatabaseError::MainNotFound)?;

        let main_symbol = main_fn.symbol;
        let main_return_type = main_fn.signature.return_type.clone();
        println!("Found main function with symbol {:?} and return type {:?}", main_symbol, main_return_type);

        println!("Generating code for all backends...");
        let compiled_output = parallax_codegen::generate_module(&hir_module)?;

        // Return the full compiled output, symbol, return type, and the HIR module
        Ok((compiled_output, main_symbol, main_return_type, hir_module))
    }
}