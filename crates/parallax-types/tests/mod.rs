use std::sync::Arc;
use std::collections::HashMap;

use miette::SourceSpan;
use parallax_resolve::{*, PrimitiveType as ResolvePrimitiveType};
use parallax_source::SourceDatabase;
use parallax_syntax::{SyntaxDatabase, ast::Literal as AstLiteral};
use parallax_types::{checker::*, context::*, types::*, PrimitiveType, *};
// Declare test modules
mod literals_operators;
mod control_flow;
mod patterns;
mod invocation;
mod aggregates;   // Added
mod definitions;  // Added
mod generics;     // Added
mod traits;       // Added
// mod definition; // Will add later
// mod generics; // Will add later 


// --- Test Setup ---
#[salsa::db]
#[derive(Default, Clone)]
pub struct DummyDb {
    storage: salsa::Storage<Self>,
}
impl salsa::Database for DummyDb {
    fn salsa_event(&self, event: &dyn Fn() -> salsa::Event) {}
}
#[salsa::db]
impl SourceDatabase for DummyDb {}
#[salsa::db]
impl SyntaxDatabase for DummyDb {}
#[salsa::db]
impl ResolveDatabase for DummyDb {}
#[salsa::db]
impl TypeDatabase for DummyDb {}

fn dummy_span() -> SourceSpan {
    SourceSpan::from((0, 0))
}

fn setup_checker() -> TypeChecker<'static> {
    let db_mock = DummyDb::default();
    let db_leaked: &'static DummyDb = Box::leak(Box::new(db_mock));
    let defs_leaked: &'static ResolvedDefinitions =
        Box::leak(Box::new(ResolvedDefinitions::default()));
    let type_ctx = TypeContext::new();
    let trait_repo = TraitRepository::new();
    TypeChecker::new(db_leaked, defs_leaked, type_ctx, trait_repo)
}

fn ty_prim(prim: PrimitiveType) -> Ty {
    Ty::with_span(TyKind::Primitive(prim), dummy_span())
}
fn ty_var(id: u32) -> Ty {
    Ty::with_span(TyKind::Var(TypeId(id)), dummy_span())
}
fn ty_named(name: &str, symbol: Option<Symbol>, args: Vec<Ty>) -> Ty {
    Ty::with_span(
        TyKind::Named {
            name: name.to_string(),
            symbol,
            args,
        },
        dummy_span(),
    )
}
fn ty_tuple(tys: Vec<Ty>) -> Ty {
    Ty::with_span(TyKind::Tuple(tys), dummy_span())
}
fn ty_array(elem_ty: Ty, size: usize) -> Ty {
    Ty::with_span(TyKind::Array(Arc::new(elem_ty), size), dummy_span())
}
fn ty_map(key: Ty, val: Ty) -> Ty {
    Ty::with_span(TyKind::Map(Arc::new(key), Arc::new(val)), dummy_span())
}
fn ty_set(elem: Ty) -> Ty {
    Ty::with_span(TyKind::Set(Arc::new(elem)), dummy_span())
}

fn resolved_lit_int(val: i128) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::Literal(AstLiteral::Int {
            value: val,
            suffix: None,
        }),
        span: dummy_span(),
        resolved_type: ResolvedType::IntegerLiteral,
    }
}
fn resolved_lit_bool(val: bool) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::Literal(AstLiteral::Bool(val)),
        span: dummy_span(),
        resolved_type: ResolvedType::Primitive(ResolvePrimitiveType::Bool),
    }
}
fn resolved_lit_string(val: &str) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::Literal(AstLiteral::String(val.to_string())),
        span: dummy_span(),
        resolved_type: ResolvedType::Primitive(ResolvePrimitiveType::String),
    }
}
fn resolved_lit_float(val: f64) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::Literal(AstLiteral::Float {
            value: val,
            suffix: None,
        }),
        span: dummy_span(),
        resolved_type: ResolvedType::FloatLiteral,
    }
}

// Helper function to create a ResolvedExpr int literal with a suffix
#[allow(dead_code)] // Keep helper even if unused for now
fn resolved_lit_int_suffix(val: i128, suffix: &str, rt: ResolvedType) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::Literal(AstLiteral::Int {
            value: val,
            suffix: Some(suffix.to_string()),
        }),
        span: dummy_span(),
        resolved_type: rt,
    }
}

// Helper function to create a ResolvedExpr float literal with a suffix
#[allow(dead_code)] // Keep helper even if unused for now
fn resolved_lit_float_suffix(val: f64, suffix: &str, rt: ResolvedType) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::Literal(AstLiteral::Float {
            value: val,
            suffix: Some(suffix.to_string()),
        }),
        span: dummy_span(),
        resolved_type: rt,
    }
}


// --- Expression Construction Helpers ---

fn resolved_binary_expr(lhs: ResolvedExpr, op: parallax_syntax::ast::BinaryOp, rhs: ResolvedExpr) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::Binary { left: Box::new(lhs), op, right: Box::new(rhs) },
        span: dummy_span(), // Combine spans later if needed
        resolved_type: ResolvedType::Unknown,
    }
}

fn resolved_unary_expr(op: parallax_syntax::ast::UnaryOp, operand: ResolvedExpr) -> ResolvedExpr {
    ResolvedExpr {
        kind: ResolvedExprKind::Unary { op, expr: Box::new(operand) },
        span: dummy_span(), // Combine spans later if needed
        resolved_type: ResolvedType::Unknown,
    }
}


// --- Trait Setup Helpers ---

// Helper to setup a basic trait and impl (e.g., Add for i32)
#[allow(dead_code)] // Keep function even if unused for now
fn add_simple_binary_trait_impl(
    checker: &mut TypeChecker,
    trait_name: &'static str,
    method_name: &'static str,
    param_ty: Ty,
    return_ty: Ty,
    impl_ty: Ty,
    trait_symbol: Symbol,
    method_symbol: Symbol,
    impl_method_symbol: Symbol,
) {
    // Define Trait
    let trait_id = checker.trait_repo.next_trait_id(); // Assign ID dynamically
    let mut trait_def = TraitDef {
        id: trait_id, // Use assigned ID
        trait_symbol,
        name: trait_name.to_string(),
        generic_params: vec![],
        methods: HashMap::new(),
        associated_types: HashMap::new(), // Simplified
        span: dummy_span(),
    };
    let method_sig = FunctionSignature {
        name: method_name.to_string(),
        self_param: Some(SelfParamKind::Value),
        generic_params: vec![],
        params: vec![ParamType { name: "rhs".to_string(), ty: param_ty.clone(), span: dummy_span() }],
        return_type: return_ty.clone(),
        span: dummy_span(),
    };
    trait_def.methods.insert(method_symbol, TraitMethod { method_symbol, name: method_name.to_string(), signature: method_sig });
    checker.trait_repo.add_trait(trait_def); // Add the trait definition

    // Define Impl
    let impl_id = checker.trait_repo.next_impl_id(); // Get ID before moving
    let mut impl_def = ImplDef {
        id: impl_id,
        impl_symbol: Symbol::fresh(), // Impl block needs its own symbol
        trait_ref: Some(TraitRef { trait_id, type_arguments: vec![], span: dummy_span() }), // Use assigned trait_id
        implementing_type: impl_ty.clone(),
        generic_params: vec![],
        methods: HashMap::new(),
        associated_type_bindings: HashMap::new(),
        span: dummy_span(),
    };
    impl_def.methods.insert(method_symbol, impl_method_symbol);
    checker.trait_repo.add_impl(impl_def);

    // Define Impl Method Signature in TypeContext
    let impl_method_sig = FunctionSignature {
        name: method_name.to_string(),
        self_param: Some(SelfParamKind::Value),
        generic_params: vec![],
        params: vec![ParamType { name: "rhs".to_string(), ty: param_ty, span: dummy_span() }],
        return_type: return_ty,
        span: dummy_span(),
    };
    checker.type_ctx.add_type(impl_method_symbol, format!("impl_{}_{}", trait_name, method_name), TypeDef::Function(impl_method_sig));
}

// Helper for Unary traits (Neg, Not)
#[allow(dead_code)] // Keep function even if unused for now
fn add_simple_unary_trait_impl(
    checker: &mut TypeChecker,
    trait_name: &'static str,
    method_name: &'static str,
    return_ty: Ty,
    impl_ty: Ty,
    trait_symbol: Symbol,
    method_symbol: Symbol,
    impl_method_symbol: Symbol,
) {
    // Define Trait
    let trait_id = checker.trait_repo.next_trait_id(); // Assign ID dynamically
    let mut trait_def = TraitDef {
        id: trait_id, // Use assigned ID
        trait_symbol,
        name: trait_name.to_string(),
        generic_params: vec![],
        methods: HashMap::new(),
        associated_types: HashMap::new(),
        span: dummy_span(),
    };
    let method_sig = FunctionSignature {
        name: method_name.to_string(),
        self_param: Some(SelfParamKind::Value),
        generic_params: vec![],
        params: vec![], // No explicit params for unary ops
        return_type: return_ty.clone(),
        span: dummy_span(),
    };
    trait_def.methods.insert(method_symbol, TraitMethod { method_symbol, name: method_name.to_string(), signature: method_sig });
    checker.trait_repo.add_trait(trait_def); // Add trait definition

    // Define Impl
    let impl_id = checker.trait_repo.next_impl_id();
    let mut impl_def = ImplDef {
        id: impl_id,
        impl_symbol: Symbol::fresh(),
        trait_ref: Some(TraitRef { trait_id, type_arguments: vec![], span: dummy_span() }), // Use assigned trait_id
        implementing_type: impl_ty.clone(),
        generic_params: vec![],
        methods: HashMap::new(),
        associated_type_bindings: HashMap::new(),
        span: dummy_span(),
    };
    impl_def.methods.insert(method_symbol, impl_method_symbol);
    checker.trait_repo.add_impl(impl_def);

    // Define Impl Method Signature
    let impl_method_sig = FunctionSignature {
        name: method_name.to_string(),
        self_param: Some(SelfParamKind::Value),
        generic_params: vec![],
        params: vec![],
        return_type: return_ty.clone(), // Use the variable `return_ty` passed into the function
        span: dummy_span(),
    };
    checker.type_ctx.add_type(impl_method_symbol, format!("impl_{}_{}", trait_name, method_name), TypeDef::Function(impl_method_sig));
}

// --- End Setup ---

// --- Pattern Construction Helpers ---

fn pat_wildcard() -> ResolvedPattern {
    ResolvedPattern {
        kind: ResolvedPatternKind::Wildcard,
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    }
}

fn pat_ident(name: &str) -> ResolvedPattern {
    ResolvedPattern {
        kind: ResolvedPatternKind::Identifier(name.to_string()),
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    }
}

fn pat_lit_int(val: i128) -> ResolvedPattern {
    ResolvedPattern {
        kind: ResolvedPatternKind::Literal(AstLiteral::Int { value: val, suffix: None }),
        span: dummy_span(),
        resolved_type: ResolvedType::IntegerLiteral,
    }
}

fn pat_lit_bool(val: bool) -> ResolvedPattern {
    ResolvedPattern {
        kind: ResolvedPatternKind::Literal(AstLiteral::Bool(val)),
        span: dummy_span(),
        resolved_type: ResolvedType::Primitive(ResolvePrimitiveType::Bool),
    }
}

fn pat_lit_string(val: &str) -> ResolvedPattern {
    ResolvedPattern {
        kind: ResolvedPatternKind::Literal(AstLiteral::String(val.to_string())),
        span: dummy_span(),
        resolved_type: ResolvedType::Primitive(ResolvePrimitiveType::String),
    }
}

fn pat_tuple(pats: Vec<ResolvedPattern>) -> ResolvedPattern {
    ResolvedPattern {
        kind: ResolvedPatternKind::Tuple(pats),
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    }
}

fn pat_array(pats: Vec<ResolvedPattern>) -> ResolvedPattern {
    ResolvedPattern {
        kind: ResolvedPatternKind::Array(pats),
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    }
}

fn pat_rest() -> ResolvedPattern {
    ResolvedPattern {
        kind: ResolvedPatternKind::Rest,
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    }
}

fn pat_or(left: ResolvedPattern, right: ResolvedPattern) -> ResolvedPattern {
    ResolvedPattern {
        kind: ResolvedPatternKind::Or(Box::new(left), Box::new(right)),
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    }
}

fn pat_struct(sym: Symbol, fields: Vec<ResolvedPatternField>) -> ResolvedPattern {
    ResolvedPattern {
        kind: ResolvedPatternKind::Struct { struct_symbol: sym, fields },
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    }
}

fn pat_field(name: &str, pat: Option<ResolvedPattern>) -> ResolvedPatternField {
    ResolvedPatternField {
        name: name.to_string(),
        pattern: pat,
        span: dummy_span(),
    }
}

// Note: ResolvedPatternKind::Constructor uses ResolvedPattern for args.
// This often means using pat_tuple for variants with multiple args, pat_ident for single binding, etc.
fn pat_constructor(sym: Symbol, args: ResolvedPattern) -> ResolvedPattern {
    ResolvedPattern {
        kind: ResolvedPatternKind::Constructor { symbol: sym, args: Box::new(args) },
        span: dummy_span(),
        resolved_type: ResolvedType::Unknown,
    }
}

// --- Type Def Setup Helpers (for struct/enum patterns) ---

// Helper to add a simple struct definition to the checker's context
#[allow(dead_code)] // Keep helper even if unused
fn add_test_struct_def(
    checker: &mut TypeChecker,
    name: &str,
    struct_symbol: Symbol,
    fields: Vec<(String, Symbol, Ty)>,
) {
    let struct_fields = fields
        .into_iter()
        .map(|(n, s, t)| Field { name: n, symbol: s, ty: t, span: dummy_span() })
        .collect();

    let struct_def = StructDef {
        name: name.to_string(),
        symbol: struct_symbol,
        generic_params: vec![],
        fields: struct_fields,
        span: dummy_span(),
    };
    checker.type_ctx.add_type(struct_symbol, name.to_string(), TypeDef::Struct(struct_def));
}

// Helper to add a simple enum definition to the checker's context
#[allow(dead_code)] // Keep helper even if unused
fn add_test_enum_def(
    checker: &mut TypeChecker,
    name: &str,
    enum_symbol: Symbol,
    variants: Vec<(String, Symbol, Vec<(String, Symbol, Ty)>)>,
) {
    let enum_variants = variants
        .into_iter()
        .map(|(v_name, v_symbol, v_fields)| {
            let fields = v_fields
                .into_iter()
                .map(|(f_name, f_symbol, f_ty)| Field { name: f_name, symbol: f_symbol, ty: f_ty, span: dummy_span() })
                .collect();
            EnumVariant { name: v_name, symbol: v_symbol, fields, span: dummy_span() }
        })
        .collect();

    let enum_def = EnumDef {
        name: name.to_string(),
        symbol: enum_symbol,
        generic_params: vec![],
        variants: enum_variants,
        span: dummy_span(),
    };

    // Add enum def
    checker.type_ctx.add_type(enum_symbol, name.to_string(), TypeDef::Enum(enum_def.clone()));

    // Add variant symbols to name mapping (needed for resolve_variant_symbol_to_names)
    for variant in enum_def.variants {
        checker.type_ctx.add_symbol_name(variant.symbol, variant.name);
    }
}


// --- Trait Setup Helpers ---