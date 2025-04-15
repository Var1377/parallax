use parallax_types::types::{Ty, TyKind, TypedDefinitions, TypedEnum, TypedExpr, TypedExprKind, TypedFunction, TypedModule, TypedStruct};
use parallax_resolve::types::Symbol;
use miette::SourceSpan;
use std::collections::HashMap;

// --- Test Helpers ---

pub fn dummy_span() -> SourceSpan {
    SourceSpan::from((0, 0))
}

pub fn dummy_ty(kind: TyKind) -> Ty {
    Ty { kind, span: Some(dummy_span()) }
}

/// Creates a simple TypedModule for testing.
/// TODO: Expand this to be more flexible.
pub fn create_typed_module(
    functions: HashMap<Symbol, TypedFunction>,
    entry_point: Option<Symbol>,
) -> TypedModule {
    TypedModule {
        definitions: TypedDefinitions {
            functions,
            structs: HashMap::new(), // Add structs/enums later if needed
            enums: HashMap::new(),
        }, // Remove Box::new()
        entry_point,
        trait_repo: Default::default(), // Add missing fields
        intrinsics: Default::default(), // Add missing fields
        errors: Default::default(),     // Add missing fields
        // ..Default::default() // Using default() might be simpler if many fields
    }
}

/// Creates a TypedModule with specific definitions.
pub fn create_typed_module_with_defs(
    functions: HashMap<Symbol, TypedFunction>,
    structs: HashMap<Symbol, TypedStruct>,
    enums: HashMap<Symbol, TypedEnum>,
    entry_point: Option<Symbol>,
) -> TypedModule {
    TypedModule {
        // name: "test_module".to_string(), // Remove incorrect field
        definitions: TypedDefinitions { functions, structs, enums }, // Remove Box::new()
        entry_point,
        trait_repo: Default::default(), // Add missing fields
        intrinsics: Default::default(), // Add missing fields
        errors: Default::default(),     // Add missing fields
    }
}

// Add missing helper function
pub fn dummy_expr(kind: TypedExprKind, ty_kind: TyKind) -> TypedExpr {
    TypedExpr {
        kind,
        ty: dummy_ty(ty_kind), // Depends on dummy_ty
        span: dummy_span(),    // Depends on dummy_span
    }
}