use miette::SourceSpan;
use std::collections::HashMap;
use lazy_static::lazy_static;
use parallax_syntax::ast::{BinaryOp, UnaryOp};
pub use crate::types::{
    Ty, TyKind, PrimitiveType, TypedExpr, TypedExprKind, FunctionSignature, TraitMethod, ImplDef, TraitDef,
    TraitRef, AssociatedTypeDef, 
};
pub use crate::context::{
    trait_repo::{self, TraitRepository, TraitId, output_assoc_type_symbol}, // Adjusted context imports - TraitId from trait_repo
    inference::InferenceContext, // Import InferenceContext directly
};
use parallax_resolve::types::{ResolvedArgument, ResolvedExpr};

use super::invocation::check_explicit_method_call; // Use method call checker
use super::expr::type_check_expression; // Import the correct function for checking expressions
use super::TypeChecker;
use crate::error::TypeError; // Import display_type helper and TypeError

// --- Operator Trait Mapping ---

// Mappings from binary operators to their corresponding trait and method names
lazy_static! {
    static ref BIN_OP_TRAIT_MAP: HashMap<BinaryOp, (&'static str, &'static str)> = {
        let mut m = HashMap::new();
        // Arithmetic
        m.insert(BinaryOp::Add, ("Add", "add"));
        m.insert(BinaryOp::Sub, ("Sub", "sub"));
        m.insert(BinaryOp::Mul, ("Mul", "mul"));
        m.insert(BinaryOp::Div, ("Div", "div"));
        m.insert(BinaryOp::Rem, ("Rem", "rem")); // Changed from Mod
        // Comparison (Result in Bool)
        m.insert(BinaryOp::Eq, ("PartialEq", "eq"));
        m.insert(BinaryOp::Ne, ("PartialEq", "ne"));
        m.insert(BinaryOp::Lt, ("PartialOrd", "lt"));
        m.insert(BinaryOp::Le, ("PartialOrd", "le"));
        m.insert(BinaryOp::Gt, ("PartialOrd", "gt"));
        m.insert(BinaryOp::Ge, ("PartialOrd", "ge"));
        // Bitwise
        m.insert(BinaryOp::BitAnd, ("BitAnd", "bitand"));
        m.insert(BinaryOp::BitOr, ("BitOr", "bitor"));
        m.insert(BinaryOp::BitXor, ("BitXor", "bitxor"));
        m.insert(BinaryOp::Shl, ("Shl", "shl"));
        m.insert(BinaryOp::Shr, ("Shr", "shr"));
        // Logical AND/OR are handled specially (removed from map)
        m
    };
}

// Mappings from unary operators to their corresponding trait and method names
lazy_static! {
    static ref UN_OP_TRAIT_MAP: HashMap<UnaryOp, (&'static str, &'static str)> = {
        let mut m = HashMap::new();
        m.insert(UnaryOp::Neg, ("Neg", "neg")); // `-` (Arithmetic negation)
        m.insert(UnaryOp::Not, ("Not", "not")); // `!` (Logical or Bitwise negation)
        m
    };
}

// --- Binary Operation Checking ---

/// Check a binary operation using trait dispatch.
pub(super) fn check_binary(
    checker: &mut TypeChecker,
    lhs: &ResolvedExpr,
    op: &BinaryOp,
    rhs: &ResolvedExpr,
    span: SourceSpan,
) -> Result<TypedExpr, TypeError> {
    // Use type_check_expression from expr module
    let typed_lhs = type_check_expression(checker, lhs, None)?;
    let typed_rhs = type_check_expression(checker, rhs, None)?;

    // Handle logical operators separately (short-circuiting)
    if *op == BinaryOp::And || *op == BinaryOp::Or { // Dereference op for comparison
        let bool_ty = Ty::new(TyKind::Primitive(PrimitiveType::Bool));
        // Use unify instead of expect_type
        checker.unify(&typed_lhs.ty, &bool_ty)?;
        checker.unify(&typed_rhs.ty, &bool_ty)?;
        // Construct TypedExpr using struct literal syntax and appropriate Kind
        let kind = match *op { // Dereference op for match
           BinaryOp::And => TypedExprKind::LogicalAnd {
                left: Box::new(typed_lhs),
                right: Box::new(typed_rhs),
            },
            BinaryOp::Or => TypedExprKind::LogicalOr {
                left: Box::new(typed_lhs),
                right: Box::new(typed_rhs),
            },
            _ => unreachable!(), // Should only be And or Or here
        };
        return Ok(TypedExpr {
            kind,
            ty: bool_ty,
            span,
        });
    }

    // Lookup the trait and method name for the operator
    let (_trait_name, method_name) = BIN_OP_TRAIT_MAP.get(op) // Use op directly (ref)
        .ok_or_else(|| TypeError::UnsupportedOperator {
            op: format!("{:?}", op), // Format the reference
            span,
        })?;

    // Prepare arguments for check_explicit_method_call
    // It expects ResolvedArguments for the method parameters (excluding self)
    let method_call_args = vec![
        ResolvedArgument { name: None, value: rhs.clone(), span: rhs.span }, // The RHS is the argument
    ];

    // Use check_explicit_method_call, passing the *receiver expression* (lhs)
    let (call_kind, result_ty) = check_explicit_method_call(
        checker,
        lhs, // Pass the original ResolvedExpr for the receiver
        method_name, // Pass the method name string
        &method_call_args, // Arguments (just `rhs`)
        span,
    )?;

    // Construct the final TypedExpr
    Ok(TypedExpr {
        kind: call_kind,
        ty: result_ty,
        span,
    })
}

// --- Unary Operation Checking ---

/// Check a unary operation using trait dispatch.
pub(super) fn check_unary(
    checker: &mut TypeChecker,
    op: &UnaryOp,
    operand: &ResolvedExpr,
    span: SourceSpan,
) -> Result<TypedExpr, TypeError> {
    // Use type_check_expression from expr module
    // let typed_operand = type_check_expression(checker, operand, None)?; // Type check operand *inside* check_explicit_method_call

    // Lookup the trait and method name for the operator
    let (_trait_name, method_name) = UN_OP_TRAIT_MAP.get(op) // Use op directly (ref)
        .ok_or_else(|| TypeError::UnsupportedOperator {
            op: format!("{:?}", op), // Format the reference
            span,
        })?;

    // Check the call using the specialized function
    // Unary ops usually take no arguments besides the receiver
    let method_call_args: Vec<ResolvedArgument> = vec![];

    // Use check_explicit_method_call, passing the operand as the receiver expression
    let (call_kind, result_ty) = check_explicit_method_call(
        checker,
        operand, // Pass the original ResolvedExpr for the receiver
        method_name, // Pass the method name string
        &method_call_args, // No arguments for unary ops
        span,
    )?;

    // Construct the final TypedExpr
    Ok(TypedExpr {
        kind: call_kind,
        ty: result_ty,
        span,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        context::{inference::{TypeEnvironment, InferenceContext}, trait_repo::{TraitRepository, TraitId}},
        types::*,
        TypeContext,
    };
    use parallax_syntax::ast::{BinaryOp, Literal, UnaryOp};
    use parallax_resolve::types::{ResolvedExpr, ResolvedExprKind, ResolvedType, Symbol, ResolvedDefinitions};
    use miette::SourceSpan;
    use std::{collections::HashMap, sync::Arc}; // Kept HashMap here as it's used locally too


    #[salsa::db]
    #[derive(Default, Clone)]
    pub struct DummyDb {
        storage: salsa::Storage<Self>,
    }

    #[salsa::db]
    impl salsa::Database for DummyDb {
        fn salsa_event(&self, event: &dyn Fn() -> salsa::Event) {
            event();
        }
    }

    #[salsa::db]
    impl parallax_source::SourceDatabase for DummyDb {}
    #[salsa::db]
    impl parallax_syntax::SyntaxDatabase for DummyDb {}
    #[salsa::db]
    impl parallax_resolve::ResolveDatabase for DummyDb {}
    #[salsa::db]
    impl crate::TypeDatabase for DummyDb {}

    // --- Test Setup Helpers ---

    fn dummy_span() -> SourceSpan {
        SourceSpan::from((0, 0))
    }

    fn i32_ty() -> Ty {
        Ty::new(TyKind::Primitive(PrimitiveType::I32))
    }

    fn bool_ty() -> Ty {
        Ty::new(TyKind::Primitive(PrimitiveType::Bool))
    }

    // Basic checker setup
    fn setup_checker<'db>(db_mock: &'db DummyDb, definitions: &'db ResolvedDefinitions) -> TypeChecker<'db> {
        let type_ctx = TypeContext::new();
        let trait_repo = TraitRepository::new();
        let inference_ctx = InferenceContext::new();
        let type_env = Arc::new(TypeEnvironment::new());

        TypeChecker {
            db: db_mock,
            definitions,
            type_ctx,
            trait_repo,
            inference_ctx,
            _type_env: type_env,
            generic_scopes: vec![],
            errors: vec![],
            needed_functions: std::cell::RefCell::new(HashMap::new()),
        }
    }

    // Helper to create a ResolvedExpr literal
    fn create_literal_expr(lit: Literal, span: SourceSpan) -> ResolvedExpr {
        ResolvedExpr {
            kind: ResolvedExprKind::Literal(lit),
            span,
            resolved_type: ResolvedType::Unknown, // Resolver would fill this
        }
    }

    // Helper to setup a basic trait and impl (e.g., Add for i32)
    fn setup_simple_trait_impl(
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
    fn setup_simple_unary_trait_impl(
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


    // --- Test Cases ---
    #[test]
    fn test_binary_add_i32() {
        let db_mock = DummyDb::default();
        let definitions = ResolvedDefinitions::default();
        let mut checker = setup_checker(&db_mock, &definitions);
        setup_simple_trait_impl(
            &mut checker,
            "Add", "add",
            i32_ty(), i32_ty(), i32_ty(),
            Symbol::new(100), Symbol::new(101), Symbol::new(102)
        );

        let span = dummy_span();
        let lhs = create_literal_expr(Literal::Int { value: 1, suffix: None }, span);
        let rhs = create_literal_expr(Literal::Int { value: 2, suffix: None }, span);
        let op = BinaryOp::Add;

        let result = check_binary(&mut checker, &lhs, &op, &rhs, span);
        assert!(result.is_ok());
        let typed_expr = result.unwrap();
        assert_eq!(typed_expr.ty, i32_ty());
        assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    }

    #[test]
    fn test_binary_eq_bool() {
        let db_mock = DummyDb::default();
        let definitions = ResolvedDefinitions::default();
        let mut checker = setup_checker(&db_mock, &definitions);
        setup_simple_trait_impl(
            &mut checker,
            "PartialEq", "eq",
            bool_ty(), bool_ty(), bool_ty(),
            Symbol::new(200), Symbol::new(201), Symbol::new(202)
        );

        let span = dummy_span();
        let lhs = create_literal_expr(Literal::Bool(true), span);
        let rhs = create_literal_expr(Literal::Bool(false), span);
        let op = BinaryOp::Eq;

        let result = check_binary(&mut checker, &lhs, &op, &rhs, span);
        assert!(result.is_ok());
        let typed_expr = result.unwrap();
        assert_eq!(typed_expr.ty, bool_ty()); // Eq returns bool
        assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    }

    #[test]
    fn test_binary_logical_and() {
        let db_mock = DummyDb::default();
        let definitions = ResolvedDefinitions::default();
        let mut checker = setup_checker(&db_mock, &definitions);
        let span = dummy_span();
        let lhs = create_literal_expr(Literal::Bool(true), span);
        let rhs = create_literal_expr(Literal::Bool(false), span);
        let op = BinaryOp::And;

        let result = check_binary(&mut checker, &lhs, &op, &rhs, span);
        assert!(result.is_ok());
        let typed_expr = result.unwrap();
        assert_eq!(typed_expr.ty, bool_ty());
        // Logical ops have their own TypedExprKind, not Call
        assert!(matches!(typed_expr.kind, TypedExprKind::LogicalAnd { .. }));
    }

    #[test]
    fn test_unary_neg_i32() {
        let db_mock = DummyDb::default();
        let definitions = ResolvedDefinitions::default();
        let mut checker = setup_checker(&db_mock, &definitions);
        setup_simple_unary_trait_impl(
            &mut checker,
            "Neg", "neg",
            i32_ty(), i32_ty(),
            Symbol::new(300), Symbol::new(301), Symbol::new(302)
        );

        let span = dummy_span();
        let operand = create_literal_expr(Literal::Int { value: 5, suffix: None }, span);
        let op = UnaryOp::Neg;

        let result = check_unary(&mut checker, &op, &operand, span);
        assert!(result.is_ok());
        let typed_expr = result.unwrap();
        assert_eq!(typed_expr.ty, i32_ty());
        assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    }

    #[test]
    fn test_unary_not_bool() {
        let db_mock = DummyDb::default();
        let definitions = ResolvedDefinitions::default();
        let mut checker = setup_checker(&db_mock, &definitions);
        setup_simple_unary_trait_impl(
            &mut checker,
            "Not", "not",
            bool_ty(), bool_ty(),
            Symbol::new(400), Symbol::new(401), Symbol::new(402)
        );

        let span = dummy_span();
        let operand = create_literal_expr(Literal::Bool(true), span);
        let op = UnaryOp::Not;

        let result = check_unary(&mut checker, &op, &operand, span);
        assert!(result.is_ok());
        let typed_expr = result.unwrap();
        assert_eq!(typed_expr.ty, bool_ty());
        assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    }

    #[test]
    fn test_binary_missing_trait_impl() {
        let db_mock = DummyDb::default();
        let definitions = ResolvedDefinitions::default();
        let mut checker = setup_checker(&db_mock, &definitions);
        // NO trait setup

        let span = dummy_span();
        let lhs = create_literal_expr(Literal::Int { value: 1, suffix: None }, span);
        let rhs = create_literal_expr(Literal::Int { value: 2, suffix: None }, span);
        let op = BinaryOp::Add;

        let result = check_binary(&mut checker, &lhs, &op, &rhs, span);
        assert!(result.is_err());
        // Further adjusted error matching pattern
        assert!(match result.err().unwrap() {
            TypeError::NoMatchingMethod { .. } => true,
            TypeError::InternalError{ message, ..} if message.contains("Signature not found") => true,
            TypeError::AmbiguousMethodCall { .. } => true,
            _ => false,
        });
    }

    #[test]
    fn test_binary_type_mismatch() {
        let db_mock = DummyDb::default();
        let definitions = ResolvedDefinitions::default();
        let mut checker = setup_checker(&db_mock, &definitions);
        setup_simple_trait_impl(
            &mut checker,
            "Add", "add",
            i32_ty(), i32_ty(), i32_ty(),
            Symbol::new(100), Symbol::new(101), Symbol::new(102)
        );

        let span = dummy_span();
        let lhs = create_literal_expr(Literal::Int { value: 1, suffix: None }, span);
        let rhs = create_literal_expr(Literal::Bool(false), span); // Mismatch here
        let op = BinaryOp::Add;

        let result = check_binary(&mut checker, &lhs, &op, &rhs, span);
        assert!(result.is_err());
        assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
    }

    #[test]
    fn test_binary_logical_and_ok() {
        let db_mock = DummyDb::default();
        let definitions = ResolvedDefinitions::default();
        let mut checker = setup_checker(&db_mock, &definitions);

        let lhs = create_literal_expr(Literal::Bool(true), dummy_span());
        let rhs = create_literal_expr(Literal::Bool(false), dummy_span());

        let result = check_binary(&mut checker, &lhs, &BinaryOp::And, &rhs, dummy_span());
        assert!(result.is_ok());
        let typed_expr = result.unwrap();

        assert!(matches!(typed_expr.kind, TypedExprKind::LogicalAnd { .. }));
        assert_eq!(typed_expr.ty, bool_ty());
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_binary_logical_or_ok() {
        let db_mock = DummyDb::default();
        let definitions = ResolvedDefinitions::default();
        let mut checker = setup_checker(&db_mock, &definitions);

        let lhs = create_literal_expr(Literal::Bool(true), dummy_span());
        let rhs = create_literal_expr(Literal::Bool(false), dummy_span());

        let result = check_binary(&mut checker, &lhs, &BinaryOp::Or, &rhs, dummy_span());
        assert!(result.is_ok());
        let typed_expr = result.unwrap();

        assert!(matches!(typed_expr.kind, TypedExprKind::LogicalOr { .. }));
        assert_eq!(typed_expr.ty, bool_ty());
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_binary_logical_and_mismatch_lhs() {
        let db_mock = DummyDb::default();
        let definitions = ResolvedDefinitions::default();
        let mut checker = setup_checker(&db_mock, &definitions);

        let lhs = create_literal_expr(Literal::Int { value: 1, suffix: None }, dummy_span()); // Not bool
        let rhs = create_literal_expr(Literal::Bool(false), dummy_span());

        let result = check_binary(&mut checker, &lhs, &BinaryOp::And, &rhs, dummy_span());
        assert!(result.is_err());
        assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
    }

     #[test]
    fn test_binary_logical_or_mismatch_rhs() {
        let db_mock = DummyDb::default();
        let definitions = ResolvedDefinitions::default();
        let mut checker = setup_checker(&db_mock, &definitions);

        let lhs = create_literal_expr(Literal::Bool(true), dummy_span());
        let rhs = create_literal_expr(Literal::Int { value: 0, suffix: None }, dummy_span()); // Not bool

        let result = check_binary(&mut checker, &lhs, &BinaryOp::Or, &rhs, dummy_span());
        assert!(result.is_err());
        assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
    }
}