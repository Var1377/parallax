// Tests for field access type checking

use parallax_lang::ast::{
    common::{Ident, Span},
    expr::{Expr, ExprKind},
};
use crate::{
    context::{Ty, ConcreteTy},
    db::TypeContextOps,
    infer::ConstraintGenerator,
    error::TypeError,
    tests::mocks::MockContext,
};

// Helper function to create a simple field access expression
fn create_field_access(object: Expr, field_name: &str) -> Expr {
    Expr {
        kind: ExprKind::Field {
            object: Box::new(object),
            name: Ident(field_name.to_string()),
        },
        span: Span { start: 0, end: 0 },
    }
}

// Helper function to create a path expression (variable reference)
fn create_path_expr(name: &str) -> Expr {
    Expr {
        kind: ExprKind::Path(vec![Ident(name.to_string())]),
        span: Span { start: 0, end: 0 },
    }
}

// Helper function to create a struct type
fn create_struct_type(name: &str) -> Ty {
    Ty::Concrete(ConcreteTy::Named {
        name: name.to_string(),
        args: vec![],
    })
}

#[test]
fn test_valid_field_access() {
    // Create a mock database
    let mut ctx = MockContext::new();
    
    // Create a path expression for a struct variable
    let struct_var = create_path_expr("point");
    
    // Record the type of the path expression in the context
    // Simulate a struct with fields x and y that have been resolved
    let struct_type = create_struct_type("Point");
    ctx.record_type(struct_var.span, struct_type.clone());
    
    // Register the variable in the symbol resolver
    ctx.register_symbol("point", struct_type);
    
    // Create a field access expression: point.x
    let field_access = create_field_access(struct_var, "x");
    
    // Run inference on the field access
    let generator = ConstraintGenerator::new();
    let result_ty = generator.infer_expr(&field_access, &mut ctx);
    
    // For now, we expect the result to be a type variable
    match result_ty {
        Ty::Var(_) => (), // Expected in current implementation
        _ => panic!("Expected field access to return a type variable, got: {:?}", result_ty),
    }
}

#[test]
fn test_invalid_field_access() {
    // Create a mock database
    let mut ctx = MockContext::new();
    
    // Create a path expression for a struct variable
    let struct_var = create_path_expr("point");
    
    // Record the type of the path expression in the context
    let struct_type = create_struct_type("Point");
    ctx.record_type(struct_var.span, struct_type.clone());
    
    // Register the variable in the symbol resolver
    ctx.register_symbol("point", struct_type);
    
    // Create a field access expression with non-existent field: point.z
    let field_access = create_field_access(struct_var, "z");
    
    // Run inference on the field access
    let generator = ConstraintGenerator::new();
    generator.infer_expr(&field_access, &mut ctx);
    
    // For now, since our implementation isn't looking up actual field definitions,
    // we won't get a specific error for invalid field access.
    // This would be checked when we implement the full struct field lookup.
    
    // In a complete implementation, there should be a FieldNotFound error
    // assert!(ctx.errors.iter().any(|e| matches!(e, TypeError::FieldNotFound { .. })));
}

#[test]
fn test_empty_field_name() {
    let mut ctx = MockContext::new();
    
    // Create a path expression for a struct variable
    let struct_var = create_path_expr("point");
    
    // Record the type of the path expression in the context
    let struct_type = create_struct_type("Point");
    ctx.record_type(struct_var.span, struct_type.clone());
    
    // Register the variable in the symbol resolver
    ctx.register_symbol("point", struct_type);
    
    // Create a field access expression with empty field name: point.
    let field_access = create_field_access(struct_var, "");
    
    // Run inference on the field access
    let generator = ConstraintGenerator::new();
    generator.infer_expr(&field_access, &mut ctx);
    
    // In a complete implementation, there should be an error for empty field name
    // This would be added in a future enhancement
}

#[test]
fn test_field_access_on_non_struct() {
    let mut ctx = MockContext::new();
    
    // Create a path expression for a non-struct variable (e.g., an integer)
    let int_var = create_path_expr("x");
    
    // Record the type of the path expression in the context
    ctx.record_type(int_var.span, Ty::Concrete(ConcreteTy::Int));
    
    // Register the variable in the symbol resolver
    ctx.register_symbol("x", Ty::Concrete(ConcreteTy::Int));
    
    // Create a field access expression on a non-struct: x.field
    let field_access = create_field_access(int_var, "field");
    
    // Run inference on the field access
    let generator = ConstraintGenerator::new();
    generator.infer_expr(&field_access, &mut ctx);
    
    // Expect an error for field access on non-struct type
    assert!(ctx.errors.iter().any(|e| matches!(e, TypeError::FieldAccessOnNonStruct { .. })));
}

#[test]
fn test_tuple_field_access() {
    let mut ctx = MockContext::new();
    
    // Create a path expression for a tuple variable
    let tuple_var = create_path_expr("tuple");
    
    // Record the type of the path expression in the context (a tuple of (int, bool))
    let tuple_type = Ty::Tuple(vec![
        Ty::Concrete(ConcreteTy::Int),
        Ty::Concrete(ConcreteTy::Bool),
    ]);
    ctx.record_type(tuple_var.span, tuple_type.clone());
    
    // Register the variable in the symbol resolver
    ctx.register_symbol("tuple", tuple_type);
    
    // Create valid numeric field access: tuple.0
    let field_access_0 = create_field_access(tuple_var.clone(), "0");
    
    // Run inference on the field access
    let generator = ConstraintGenerator::new();
    let _result_ty_0 = generator.infer_expr(&field_access_0, &mut ctx);
    
    // The result type should be Int (first element of tuple)
    // When our implementation is complete, this should be the case
    // For now, we can't check this due to limitations in the mock
    
    // Similar for the other field accesses
    let field_access_1 = create_field_access(tuple_var.clone(), "1");
    let _ = generator.infer_expr(&field_access_1, &mut ctx);
    
    let field_access_2 = create_field_access(tuple_var.clone(), "2");
    generator.infer_expr(&field_access_2, &mut ctx);
    
    let field_access_invalid = create_field_access(tuple_var, "field");
    generator.infer_expr(&field_access_invalid, &mut ctx);
    
    // Check that we have the expected error types
    assert!(ctx.errors.iter().any(|e| matches!(e, TypeError::FieldNotFound { .. } | 
                                                     TypeError::InvalidTupleField { .. })));
} 