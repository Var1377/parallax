use parallax_types::types::*;
use parallax_resolve::types::Symbol;
use miette::SourceSpan;
use std::collections::BTreeMap;

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
    functions: BTreeMap<Symbol, TypedFunction>,
    entry_point: Option<Symbol>,
) -> TypedModule {
    TypedModule {
        definitions: TypedDefinitions {
            functions,
            structs: BTreeMap::new(),
            enums: BTreeMap::new(),
        },
        entry_point,
        intrinsics: Default::default(),
        errors: Default::default(),
    }
}

/// Creates a TypedModule with specific definitions.
pub fn create_typed_module_with_defs(
    functions: BTreeMap<Symbol, TypedFunction>,
    structs: BTreeMap<Symbol, TypedStruct>,
    enums: BTreeMap<Symbol, TypedEnum>,
    entry_point: Option<Symbol>,
) -> TypedModule {
    TypedModule {
        definitions: TypedDefinitions { functions, structs, enums },
        entry_point,
        intrinsics: Default::default(),
        errors: Default::default(),
    }
}

// Add missing helper function
pub fn dummy_expr(kind: TypedExprKind, ty_kind: TyKind) -> TypedExpr {
    TypedExpr {
        kind,
        ty: dummy_ty(ty_kind),
        span: dummy_span(),
    }
}