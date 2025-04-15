use std::path::PathBuf;
use parallax_source::SourceDatabase;
use parallax_syntax::SyntaxDatabase;
use parallax_resolve::ResolveDatabase;
use parallax_types::TypeDatabase;
use parallax_hir::lower::lower_module_to_anf_hir;
use parallax_hir::{perform_dce, HirModule, Symbol};
use parallax_hir::hir::{HirType, ResolvePrimitiveType};
use parallax_native::CompiledArtifact;
use parallax_codegen;
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
        eprintln!("Event: {event:?}");
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

        println!("Resolved module tree warnings: {:#?}", resolved_module_tree.warnings(self));

        if resolved_module_tree.errors(self).is_empty() {
            println!("Resolution successful.");
        } else {
            panic!("Resolution errors: {:#?}", resolved_module_tree.errors(self));
        }

        let typed_module_tree = self.type_check_definitions(resolved_module_tree);

        if typed_module_tree.errors.is_empty() {
            println!("Type checking successful.");
        } else {
            panic!("Type checking errors: {:#?}", typed_module_tree.errors);
        }
        // TODO: Collect and handle diagnostics from the above steps properly

        println!("Lowering to HIR...");
        let hir = lower_module_to_anf_hir(&typed_module_tree);
        let dce_hir = perform_dce(hir);
        

        println!("DCE'ed HIR: {:#?}", dce_hir);

        Ok(dce_hir)
    }

    /// Compile the project for running, producing a native artifact, entry point symbol,
    /// and the entry point's HIR type signature.
    ///
    /// This function performs the full compilation pipeline and identifies the
    /// main entry point function, ensuring it has an allowed signature
    /// (`fn() -> i64` or `fn() -> ()`).
    pub fn compile_for_run<'db>(&'db self) -> DatabaseResult<(CompiledArtifact, Symbol, HirType)> {
        let hir_module = self.compile_to_hir()?;

        println!("Searching for main function...");
        let main_fn = hir_module.functions.iter()
            .find(|f| f.name == "main")
            .ok_or(DatabaseError::MainNotFound)?;

        // Verify signature: Must be fn() -> i64 OR fn() -> ()
        let main_signature = &main_fn.signature;
        let valid_return_type = match &main_signature.return_type {
            HirType::Primitive(ResolvePrimitiveType::I64) => true,
            HirType::Tuple(elements) if elements.is_empty() => true, // Unit type ()
            _ => false,
        };

        if !main_signature.params.is_empty() || !valid_return_type {
            // Update error to reflect allowed types
            return Err(DatabaseError::MainIncorrectSignature);
        }

        let main_symbol = main_fn.symbol;
        let main_return_type = main_signature.return_type.clone(); // Clone the type to return
        println!("Found main function with symbol {:?} and return type {:?}", main_symbol, main_return_type);

        println!("Generating native code...");
        let compiled_output = parallax_codegen::generate_module(&hir_module)?;
        let artifact = compiled_output.native_artifact;

        // Return the artifact, symbol, and the return type
        Ok((artifact, main_symbol, main_return_type))
    }
}