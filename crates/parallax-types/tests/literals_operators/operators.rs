// tests/literals_operators/operators.rs
use crate::*; // Import helpers from tests/mod.rs
use parallax_syntax::ast::{BinaryOp, UnaryOp};
use parallax_types::error::TypeError;

// --- Binary Operator Tests ---

// --- Arithmetic ---
#[test]
fn test_binary_add_i32() {
    let mut checker = setup_checker();
    add_simple_binary_trait_impl(
        &mut checker, "Add", "add",
        ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::I32),
        Symbol::new(100), Symbol::new(101), Symbol::new(102)
    );
    let lhs = resolved_lit_int(10);
    let rhs = resolved_lit_int(5);
    let expr = resolved_binary_expr(lhs, BinaryOp::Add, rhs);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::I32));
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_binary_sub_i32() {
    let mut checker = setup_checker();
    add_simple_binary_trait_impl(
        &mut checker, "Sub", "sub",
        ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::I32),
        Symbol::new(110), Symbol::new(111), Symbol::new(112)
    );
    let lhs = resolved_lit_int(10);
    let rhs = resolved_lit_int(5);
    let expr = resolved_binary_expr(lhs, BinaryOp::Sub, rhs);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::I32));
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_binary_mul_i32() {
    let mut checker = setup_checker();
    add_simple_binary_trait_impl(
        &mut checker, "Mul", "mul",
        ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::I32),
        Symbol::new(120), Symbol::new(121), Symbol::new(122)
    );
    let lhs = resolved_lit_int(10);
    let rhs = resolved_lit_int(5);
    let expr = resolved_binary_expr(lhs, BinaryOp::Mul, rhs);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::I32));
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_binary_div_i32() {
    let mut checker = setup_checker();
    add_simple_binary_trait_impl(
        &mut checker, "Div", "div",
        ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::I32),
        Symbol::new(130), Symbol::new(131), Symbol::new(132)
    );
    let lhs = resolved_lit_int(10);
    let rhs = resolved_lit_int(5);
    let expr = resolved_binary_expr(lhs, BinaryOp::Div, rhs);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::I32));
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_binary_rem_i32() {
    let mut checker = setup_checker();
    add_simple_binary_trait_impl(
        &mut checker, "Rem", "rem",
        ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::I32),
        Symbol::new(140), Symbol::new(141), Symbol::new(142)
    );
    let lhs = resolved_lit_int(10);
    let rhs = resolved_lit_int(3);
    let expr = resolved_binary_expr(lhs, BinaryOp::Rem, rhs);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::I32));
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_binary_add_f64() {
    let mut checker = setup_checker();
    add_simple_binary_trait_impl(
        &mut checker, "Add", "add",
        ty_prim(PrimitiveType::F64), ty_prim(PrimitiveType::F64), ty_prim(PrimitiveType::F64),
        Symbol::new(150), Symbol::new(151), Symbol::new(152)
    );
    let lhs = resolved_lit_float(10.5);
    let rhs = resolved_lit_float(5.2);
    let expr = resolved_binary_expr(lhs, BinaryOp::Add, rhs);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::F64));
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty());
}

// --- Comparison ---
#[test]
fn test_binary_eq_bool() {
    let mut checker = setup_checker();
    add_simple_binary_trait_impl(
        &mut checker, "PartialEq", "eq",
        ty_prim(PrimitiveType::Bool), ty_prim(PrimitiveType::Bool), ty_prim(PrimitiveType::Bool),
        Symbol::new(200), Symbol::new(201), Symbol::new(202)
    );
    let lhs = resolved_lit_bool(true);
    let rhs = resolved_lit_bool(false);
    let expr = resolved_binary_expr(lhs, BinaryOp::Eq, rhs);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::Bool));
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_binary_ne_bool() {
    let mut checker = setup_checker();
    add_simple_binary_trait_impl(
        &mut checker, "PartialEq", "ne", // Uses PartialEq trait, ne method
        ty_prim(PrimitiveType::Bool), ty_prim(PrimitiveType::Bool), ty_prim(PrimitiveType::Bool),
        Symbol::new(200), Symbol::new(203), Symbol::new(204) // Different method/impl symbols
    );
    let lhs = resolved_lit_bool(true);
    let rhs = resolved_lit_bool(true);
    let expr = resolved_binary_expr(lhs, BinaryOp::Ne, rhs);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::Bool));
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_binary_lt_i32() {
    let mut checker = setup_checker();
    add_simple_binary_trait_impl(
        &mut checker, "PartialOrd", "lt",
        ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::Bool), ty_prim(PrimitiveType::I32), // Returns bool
        Symbol::new(210), Symbol::new(211), Symbol::new(212)
    );
    let lhs = resolved_lit_int(5);
    let rhs = resolved_lit_int(10);
    let expr = resolved_binary_expr(lhs, BinaryOp::Lt, rhs);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::Bool));
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_binary_gt_i32() {
    let mut checker = setup_checker();
    add_simple_binary_trait_impl(
        &mut checker, "PartialOrd", "gt",
        ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::Bool), ty_prim(PrimitiveType::I32),
        Symbol::new(210), Symbol::new(213), Symbol::new(214)
    );
    let lhs = resolved_lit_int(15);
    let rhs = resolved_lit_int(10);
    let expr = resolved_binary_expr(lhs, BinaryOp::Gt, rhs);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::Bool));
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_binary_le_i32() {
    let mut checker = setup_checker();
    add_simple_binary_trait_impl(
        &mut checker, "PartialOrd", "le",
        ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::Bool), ty_prim(PrimitiveType::I32),
        Symbol::new(210), Symbol::new(215), Symbol::new(216)
    );
    let lhs = resolved_lit_int(10);
    let rhs = resolved_lit_int(10);
    let expr = resolved_binary_expr(lhs, BinaryOp::Le, rhs);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::Bool));
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_binary_ge_i32() {
    let mut checker = setup_checker();
    add_simple_binary_trait_impl(
        &mut checker, "PartialOrd", "ge",
        ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::Bool), ty_prim(PrimitiveType::I32),
        Symbol::new(210), Symbol::new(217), Symbol::new(218)
    );
    let lhs = resolved_lit_int(10);
    let rhs = resolved_lit_int(5);
    let expr = resolved_binary_expr(lhs, BinaryOp::Ge, rhs);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::Bool));
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_binary_lt_f64() {
    let mut checker = setup_checker();
    add_simple_binary_trait_impl(
        &mut checker, "PartialOrd", "lt",
        ty_prim(PrimitiveType::F64), ty_prim(PrimitiveType::Bool), ty_prim(PrimitiveType::F64),
        Symbol::new(220), Symbol::new(221), Symbol::new(222)
    );
    let lhs = resolved_lit_float(5.5);
    let rhs = resolved_lit_float(10.1);
    let expr = resolved_binary_expr(lhs, BinaryOp::Lt, rhs);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::Bool));
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty());
}


// --- Bitwise/Shift Tests ---
#[test]
fn test_binary_bitand_i32() {
    let mut checker = setup_checker();
    add_simple_binary_trait_impl(
        &mut checker, "BitAnd", "bitand",
        ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::I32),
        Symbol::new(500), Symbol::new(501), Symbol::new(502)
    );
    let lhs = resolved_lit_int(0b1100);
    let rhs = resolved_lit_int(0b1010);
    let expr = resolved_binary_expr(lhs, BinaryOp::BitAnd, rhs);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::I32));
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_binary_bitor_i32() {
    let mut checker = setup_checker();
    add_simple_binary_trait_impl(
        &mut checker, "BitOr", "bitor",
        ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::I32),
        Symbol::new(510), Symbol::new(511), Symbol::new(512)
    );
    let lhs = resolved_lit_int(0b1100);
    let rhs = resolved_lit_int(0b1010);
    let expr = resolved_binary_expr(lhs, BinaryOp::BitOr, rhs);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::I32));
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_binary_bitxor_i32() {
    let mut checker = setup_checker();
    add_simple_binary_trait_impl(
        &mut checker, "BitXor", "bitxor",
        ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::I32),
        Symbol::new(520), Symbol::new(521), Symbol::new(522)
    );
    let lhs = resolved_lit_int(0b1100);
    let rhs = resolved_lit_int(0b1010);
    let expr = resolved_binary_expr(lhs, BinaryOp::BitXor, rhs);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::I32));
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_binary_shl_i32() {
    let mut checker = setup_checker();
    add_simple_binary_trait_impl(
        &mut checker, "Shl", "shl",
        ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::I32), // RHS is often usize/u32 in Rust, simplify to i32 for test
        Symbol::new(530), Symbol::new(531), Symbol::new(532)
    );
    let lhs = resolved_lit_int(1);
    let rhs = resolved_lit_int(2);
    let expr = resolved_binary_expr(lhs, BinaryOp::Shl, rhs);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::I32));
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_binary_shr_i32() {
    let mut checker = setup_checker();
    add_simple_binary_trait_impl(
        &mut checker, "Shr", "shr",
        ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::I32), // RHS is often usize/u32 in Rust, simplify to i32 for test
        Symbol::new(540), Symbol::new(541), Symbol::new(542)
    );
    let lhs = resolved_lit_int(4);
    let rhs = resolved_lit_int(1);
    let expr = resolved_binary_expr(lhs, BinaryOp::Shr, rhs);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::I32));
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty());
}


// --- Logical Operator Tests ---
#[test]
fn test_binary_logical_and() {
    let mut checker = setup_checker();
    let lhs = resolved_lit_bool(true);
    let rhs = resolved_lit_bool(false);
    let expr = resolved_binary_expr(lhs, BinaryOp::And, rhs);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::Bool));
    assert!(matches!(typed_expr.kind, TypedExprKind::LogicalAnd { .. }));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_binary_logical_or() {
    let mut checker = setup_checker();
    let lhs = resolved_lit_bool(true);
    let rhs = resolved_lit_bool(false);
    let expr = resolved_binary_expr(lhs, BinaryOp::Or, rhs);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::Bool));
    assert!(matches!(typed_expr.kind, TypedExprKind::LogicalOr { .. }));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_binary_logical_and_type_mismatch() {
    let mut checker = setup_checker();
    let lhs = resolved_lit_int(1); // Not bool
    let rhs = resolved_lit_bool(false);
    let expr = resolved_binary_expr(lhs, BinaryOp::And, rhs);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_err(), "Expected Err for logical AND type mismatch");
    assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
}


// --- Error Case Tests ---
#[test]
fn test_binary_add_type_mismatch() {
    let mut checker = setup_checker();
    add_simple_binary_trait_impl(
        &mut checker, "Add", "add",
        ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::I32),
        Symbol::new(100), Symbol::new(101), Symbol::new(102)
    );
    let lhs = resolved_lit_int(10);
    let rhs = resolved_lit_bool(false); // Mismatch
    let expr = resolved_binary_expr(lhs, BinaryOp::Add, rhs);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_err(), "Expected Err, got Ok: {:?}", result.ok());
    assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. } | TypeError::NoMatchingMethod { .. } | TypeError::AmbiguousMethodCall { .. } ));
}

#[test]
fn test_binary_mul_missing_impl() {
    let mut checker = setup_checker();
    // No impl for Mul<i32>
    let lhs = resolved_lit_int(10);
    let rhs = resolved_lit_int(5);
    let expr = resolved_binary_expr(lhs, BinaryOp::Mul, rhs);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_err(), "Expected Err for missing Mul impl");
    assert!(matches!(result.err().unwrap(), TypeError::NoMatchingMethod { .. } | TypeError::AmbiguousMethodCall { .. }));
}

// --- Unary Operator Tests ---

#[test]
fn test_unary_neg_i32() {
    let mut checker = setup_checker();
    add_simple_unary_trait_impl(
        &mut checker, "Neg", "neg",
        ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::I32),
        Symbol::new(300), Symbol::new(301), Symbol::new(302)
    );
    let operand = resolved_lit_int(5);
    let expr = resolved_unary_expr(UnaryOp::Neg, operand);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::I32));
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_unary_not_bool() {
    let mut checker = setup_checker();
    add_simple_unary_trait_impl(
        &mut checker, "Not", "not",
        ty_prim(PrimitiveType::Bool), ty_prim(PrimitiveType::Bool),
        Symbol::new(400), Symbol::new(401), Symbol::new(402)
    );
    let operand = resolved_lit_bool(true);
    let expr = resolved_unary_expr(UnaryOp::Not, operand);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::Bool));
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_unary_not_i32() {
    let mut checker = setup_checker();
    // Setup Not for i32 (Bitwise Not)
    add_simple_unary_trait_impl(
        &mut checker, "Not", "not",
        ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::I32),
        Symbol::new(410), Symbol::new(411), Symbol::new(412)
    );
    let operand = resolved_lit_int(0b1100);
    let expr = resolved_unary_expr(UnaryOp::Not, operand);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_ok(), "Expected Ok, got Err: {:?}", result.err());
    let typed_expr = result.unwrap();
    assert_eq!(typed_expr.ty, ty_prim(PrimitiveType::I32));
    assert!(matches!(typed_expr.kind, TypedExprKind::Call { .. }));
    assert!(checker.errors.is_empty());
}

#[test]
fn test_unary_neg_type_mismatch() {
    let mut checker = setup_checker();
    add_simple_unary_trait_impl(
        &mut checker, "Neg", "neg",
        ty_prim(PrimitiveType::I32), ty_prim(PrimitiveType::I32),
        Symbol::new(300), Symbol::new(301), Symbol::new(302)
    );
    let operand = resolved_lit_bool(true); // Mismatch
    let expr = resolved_unary_expr(UnaryOp::Neg, operand);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_err(), "Expected Err, got Ok: {:?}", result.ok());
     assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. } | TypeError::NoMatchingMethod { .. } | TypeError::AmbiguousMethodCall { .. } ));
}

#[test]
fn test_unary_not_missing_impl() {
    let mut checker = setup_checker();
    // No impl for Not<f64>
    let operand = resolved_lit_float(1.0); // Use float helper
    let expr = resolved_unary_expr(UnaryOp::Not, operand);
    let result = checker::expr::type_check_expression(&mut checker, &expr, None);
    assert!(result.is_err(), "Expected Err for missing Not impl on float");
    assert!(matches!(result.err().unwrap(), TypeError::NoMatchingMethod { .. } | TypeError::AmbiguousMethodCall { .. }));
}

// TODO: Add tests for bitwise operators, shifts, other comparison types, more complex scenarios. 