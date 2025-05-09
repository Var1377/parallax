#![allow(unused_imports)] // Temp allow while building tests
#![allow(dead_code)] // Allow unused helper funcs/vars during dev
use parallax_hir::tests::{dummy_span, dummy_ty, create_typed_module, create_typed_module_with_defs};
use parallax_hir::hir::* ;
use parallax_hir::hir::PrimitiveType;
use parallax_hir::lower::{lower_module_to_anf_hir, flatten_hir_expr};
use parallax_hir::dce::perform_dce;
use parallax_types::types::{*, PrimitiveType as TypedPrimitiveType};
use parallax_resolve::types::Symbol;
use std::collections::BTreeMap;
use std::sync::Arc;

// --- Helper to check existence ---
fn exists_fn(module: &HirModule, name: &str) -> bool {
    module.functions.iter().any(|f| f.name == name)
}
fn exists_struct(module: &HirModule, name: &str) -> bool {
    module.structs.iter().any(|s| s.name == name)
}
fn exists_enum(module: &HirModule, name: &str) -> bool {
    module.enums.iter().any(|e| e.name == name)
}

// --- Common Types ---
fn ty_i32() -> Ty { dummy_ty(TyKind::Primitive(TypedPrimitiveType::I32)) }
fn ty_bool() -> Ty { dummy_ty(TyKind::Primitive(TypedPrimitiveType::Bool)) }
fn ty_unit() -> Ty { dummy_ty(TyKind::Primitive(TypedPrimitiveType::Unit)) }
fn ty_fn(params: Vec<Ty>, ret: Ty) -> Ty { dummy_ty(TyKind::Function(params, Arc::new(ret))) }
fn ty_named(name: &str) -> Ty { dummy_ty(TyKind::Named { name: name.to_string(), args: vec![], symbol: None })}

// --- Test Cases ---

#[test]
fn test_dce_simple_entry() {
    // main() { 1 } -> should keep main
    let main_sym = Symbol::new(1);
    let mut functions = BTreeMap::new();
    functions.insert(main_sym, TypedFunction {
        name: "main".to_string(), 
        params: vec![], // No params
        return_type: ty_i32(),
        body: Some(TypedExpr { kind: TypedExprKind::IntLiteral { value: 1, suffix: None }, ty: ty_i32(), span: dummy_span() }),
        generic_params: vec![], span: dummy_span(), is_effectful: false,
    });

    let typed_module = create_typed_module(functions, Some(main_sym));
    let hir_module = lower_module_to_anf_hir(&typed_module);
    let dce_module = perform_dce(hir_module);

    assert_eq!(dce_module.functions.len(), 1);
    assert!(exists_fn(&dce_module, "main"));
}

#[test]
fn test_dce_direct_call() {
    // main() { callee() }; callee() { 1 } -> should keep main and callee
    let main_sym = Symbol::new(1);
    let callee_sym = Symbol::new(2);
    let mut functions = BTreeMap::new();
    let fn_type = ty_fn(vec![], ty_i32());

    functions.insert(callee_sym, TypedFunction {
        name: "callee".to_string(), 
        params: vec![], // No params
        return_type: ty_i32(),
        body: Some(TypedExpr { kind: TypedExprKind::IntLiteral { value: 1, suffix: None }, ty: ty_i32(), span: dummy_span() }),
        generic_params: vec![], span: dummy_span(), is_effectful: false,
    });
    functions.insert(main_sym, TypedFunction {
        name: "main".to_string(), 
        params: vec![], // No params
        return_type: ty_i32(),
        body: Some(TypedExpr {
            kind: TypedExprKind::Call {
                func_expr: Box::new(TypedExpr { kind: TypedExprKind::Variable { symbol: callee_sym, name: "callee".to_string() }, ty: fn_type, span: dummy_span() }),
                func_symbol: Some(callee_sym),
                type_args: None,
                args: vec![],
            },
            ty: ty_i32(), span: dummy_span(),
        }),
        generic_params: vec![], span: dummy_span(), is_effectful: false,
    });

    let typed_module = create_typed_module(functions, Some(main_sym));
    let hir_module = lower_module_to_anf_hir(&typed_module);
    let dce_module = perform_dce(hir_module);

    assert_eq!(dce_module.functions.len(), 2);
    assert!(exists_fn(&dce_module, "main"));
    assert!(exists_fn(&dce_module, "callee"));
}

#[test]
fn test_dce_indirect_call() {
    // main->A->B; B returns 1. Keep main, A, B.
    let main_sym = Symbol::new(1);
    let a_sym = Symbol::new(2);
    let b_sym = Symbol::new(3);
    let mut functions = BTreeMap::new();
    let fn_type = ty_fn(vec![], ty_i32());

    functions.insert(b_sym, TypedFunction { name: "B".to_string(), params: vec![], return_type: ty_i32(), body: Some(TypedExpr { kind: TypedExprKind::IntLiteral { value: 1, suffix: None }, ty: ty_i32(), span: dummy_span() }), generic_params: vec![], span: dummy_span(), is_effectful: false });
    functions.insert(a_sym, TypedFunction { name: "A".to_string(), params: vec![], return_type: ty_i32(), body: Some(TypedExpr { kind: TypedExprKind::Call { func_expr: Box::new(TypedExpr { kind: TypedExprKind::Variable { symbol: b_sym, name: "B".to_string() }, ty: fn_type.clone(), span: dummy_span() }), func_symbol: Some(b_sym), type_args: None, args: vec![] }, ty: ty_i32(), span: dummy_span() }), generic_params: vec![], span: dummy_span(), is_effectful: false });
    functions.insert(main_sym, TypedFunction { name: "main".to_string(), params: vec![], return_type: ty_i32(), body: Some(TypedExpr { kind: TypedExprKind::Call { func_expr: Box::new(TypedExpr { kind: TypedExprKind::Variable { symbol: a_sym, name: "A".to_string() }, ty: fn_type.clone(), span: dummy_span() }), func_symbol: Some(a_sym), type_args: None, args: vec![] }, ty: ty_i32(), span: dummy_span() }), generic_params: vec![], span: dummy_span(), is_effectful: false });

    let typed_module = create_typed_module(functions, Some(main_sym));
    let hir_module = lower_module_to_anf_hir(&typed_module);
    let dce_module = perform_dce(hir_module);

    assert_eq!(dce_module.functions.len(), 3);
    assert!(exists_fn(&dce_module, "main"));
    assert!(exists_fn(&dce_module, "A"));
    assert!(exists_fn(&dce_module, "B"));
}

#[test]
fn test_dce_unused_function() {
    // main() { 1 }; unused() { 2 } -> should keep main, remove unused
    let main_sym = Symbol::new(1);
    let unused_sym = Symbol::new(2);
    let mut functions = BTreeMap::new();

    functions.insert(main_sym, TypedFunction { name: "main".to_string(), params: vec![], return_type: ty_i32(), body: Some(TypedExpr { kind: TypedExprKind::IntLiteral { value: 1, suffix: None }, ty: ty_i32(), span: dummy_span() }), generic_params: vec![], span: dummy_span(), is_effectful: false });
    functions.insert(unused_sym, TypedFunction { name: "unused".to_string(), params: vec![], return_type: ty_i32(), body: Some(TypedExpr { kind: TypedExprKind::IntLiteral { value: 2, suffix: None }, ty: ty_i32(), span: dummy_span() }), generic_params: vec![], span: dummy_span(), is_effectful: false });

    let typed_module = create_typed_module(functions, Some(main_sym));
    let hir_module = lower_module_to_anf_hir(&typed_module);
    let dce_module = perform_dce(hir_module);

    assert_eq!(dce_module.functions.len(), 1);
    assert!(exists_fn(&dce_module, "main"));
    assert!(!exists_fn(&dce_module, "unused"));
}

#[test]
fn test_dce_struct_used() {
    // struct Point {x: i32}; main() { Point { x: 1 }.x } -> keep main, Point
    let main_sym = Symbol::new(1);
    let point_sym = Symbol::new(10);
    let field_x_sym = Symbol::new(11);
    let var_p_sym = Symbol::new(2);

    let mut functions = BTreeMap::new();
    let mut structs = BTreeMap::new();

    structs.insert(point_sym, TypedStruct { 
        symbol: point_sym, 
        name: "Point".to_string(), 
        fields: vec![], 
        generic_params: vec![], 
        span: dummy_span() 
    });

    let point_constructor = TypedExpr { kind: TypedExprKind::Struct { 
        name: "Point".to_string(), 
        fields: vec![], 
        base: None,
        struct_symbol: point_sym,
    }, ty: ty_named("Point"), span: dummy_span() };
    let proj_x = TypedExpr { kind: TypedExprKind::Field { object: Box::new(TypedExpr { kind: TypedExprKind::Variable { symbol: var_p_sym, name: "p".to_string() }, ty: ty_named("Point"), span: dummy_span() }), field_name: "x".to_string(), field_symbol: field_x_sym }, ty: ty_i32(), span: dummy_span() };
    let main_body = TypedExpr { kind: TypedExprKind::Block(vec![point_constructor, proj_x]), ty: ty_i32(), span: dummy_span() };
    functions.insert(main_sym, TypedFunction { name: "main".to_string(), params: vec![], return_type: ty_i32(), body: Some(main_body), generic_params: vec![], span: dummy_span(), is_effectful: false });

    let typed_module = create_typed_module_with_defs(functions, structs, BTreeMap::new(), Some(main_sym));
    let hir_module = lower_module_to_anf_hir(&typed_module);
    let dce_module = perform_dce(hir_module);

    assert_eq!(dce_module.functions.len(), 1);
    assert!(exists_fn(&dce_module, "main"));
    assert_eq!(dce_module.structs.len(), 1);
    assert!(exists_struct(&dce_module, "Point"));
}

#[test]
fn test_dce_struct_unused() {
    // struct Point {x: i32}; main() { 1 } -> keep main, remove Point
    let main_sym = Symbol::new(1);
    let point_sym = Symbol::new(10);
    let field_x_sym = Symbol::new(11);
    let mut functions = BTreeMap::new();
    let mut structs = BTreeMap::new();

    structs.insert(point_sym, TypedStruct { 
        symbol: point_sym, 
        name: "Point".to_string(), 
        fields: vec![], 
        generic_params: vec![], 
        span: dummy_span() 
    });
    functions.insert(main_sym, TypedFunction { name: "main".to_string(), params: vec![], return_type: ty_i32(), body: Some(TypedExpr { kind: TypedExprKind::IntLiteral { value: 1, suffix: None }, ty: ty_i32(), span: dummy_span() }), generic_params: vec![], span: dummy_span(), is_effectful: false });

    let typed_module = create_typed_module_with_defs(functions, structs, BTreeMap::new(), Some(main_sym));
    let hir_module = lower_module_to_anf_hir(&typed_module);
    let dce_module = perform_dce(hir_module);

    assert_eq!(dce_module.functions.len(), 1);
    assert!(exists_fn(&dce_module, "main"));
    assert_eq!(dce_module.structs.len(), 0);
    assert!(!exists_struct(&dce_module, "Point"));
}

#[test]
fn test_dce_enum_used_variant() {
    // enum Opt { Some(i32), None }; main() { match Opt::Some(1) { Some(x)=>x, _=>0 } } -> keep main, Opt
    let main_sym = Symbol::new(1);
    let opt_sym = Symbol::new(10);
    let some_sym = Symbol::new(11);
    let none_sym = Symbol::new(12);
    let var_o_sym = Symbol::new(2);
    let var_x_sym = Symbol::new(3);

    let mut functions = BTreeMap::new();
    let mut enums = BTreeMap::new();

    enums.insert(opt_sym, TypedEnum { 
        symbol: opt_sym, 
        name: "Opt".to_string(), 
        variants: vec![
            TypedVariant { 
                name: "Some".to_string(), 
                symbol: some_sym, 
                fields: vec![TypedField {
                    name: "0".to_string(),
                    symbol: Symbol::new(100), // Using a new symbol for the field
                    ty: ty_i32(),
                    is_public: true,
                    span: dummy_span()
                }],
                span: dummy_span() 
            }, 
            TypedVariant { 
                name: "None".to_string(), 
                symbol: none_sym, 
                fields: vec![], // Unit variant has no fields
                span: dummy_span() 
            }
        ], 
        generic_params: vec![], 
        span: dummy_span() 
    });

    let let_o = TypedExpr { kind: TypedExprKind::Let { 
        pattern: TypedPattern { 
            kind: TypedPatternKind::Identifier { 
                symbol: var_o_sym, 
                name: "o".to_string() 
            }, 
            ty: ty_named("Opt"), 
            span: dummy_span() 
        }, 
        value: Box::new(TypedExpr { 
            kind: TypedExprKind::VariantConstructor { 
                enum_name: "Opt".to_string(), 
                variant_name: "Some".to_string(), 
                args: vec![TypedArgument { 
                    name: None, 
                    value: TypedExpr { 
                        kind: TypedExprKind::IntLiteral { 
                            value: 1, 
                            suffix: None 
                        }, 
                        ty: ty_i32(), 
                        span: dummy_span()
                    }, 
                    span: dummy_span() 
                }],
                enum_symbol: opt_sym,
                variant_symbol: some_sym,
            }, 
            ty: ty_named("Opt"), 
            span: dummy_span() 
        }) 
    }, ty: ty_unit(), span: dummy_span() };
    let match_o = TypedExpr { kind: TypedExprKind::Match { 
        scrutinee: Box::new(TypedExpr { kind: TypedExprKind::Variable { symbol: var_o_sym, name: "o".to_string() }, ty: ty_named("Opt"), span: dummy_span() }), 
        arms: vec![ TypedMatchArm { 
            pattern: TypedPattern { 
                kind: TypedPatternKind::Constructor { 
                    enum_name: "Opt".to_string(), 
                    variant_name: "Some".to_string(), 
                    args: vec![TypedPatternArgument::Positional(TypedPattern { 
                        kind: TypedPatternKind::Identifier { symbol: var_x_sym, name: "x".to_string() }, 
                        ty: ty_i32(), 
                        span: dummy_span() 
                    })],
                    enum_symbol: opt_sym,
                    variant_symbol: some_sym,
                }, 
                ty: ty_named("Opt"), 
                span: dummy_span() 
            }, 
            body: TypedExpr { kind: TypedExprKind::Variable { symbol: var_x_sym, name: "x".to_string() }, ty: ty_i32(), span: dummy_span() } 
        }, TypedMatchArm { 
            pattern: TypedPattern { kind: TypedPatternKind::Wildcard, ty: ty_named("Opt"), span: dummy_span() }, 
            body: TypedExpr { kind: TypedExprKind::IntLiteral { value: 0, suffix: None }, ty: ty_i32(), span: dummy_span() } 
        } ] 
    }, ty: ty_i32(), span: dummy_span() };
    let main_body = TypedExpr { kind: TypedExprKind::Block(vec![let_o, match_o]), ty: ty_i32(), span: dummy_span() };
    functions.insert(main_sym, TypedFunction { name: "main".to_string(), params: vec![], return_type: ty_i32(), body: Some(main_body), generic_params: vec![], span: dummy_span(), is_effectful: false });

    let typed_module = create_typed_module_with_defs(functions, BTreeMap::new(), enums, Some(main_sym));
    let hir_module = lower_module_to_anf_hir(&typed_module);
    let dce_module = perform_dce(hir_module);

    assert_eq!(dce_module.functions.len(), 1);
    assert!(exists_fn(&dce_module, "main"));
    assert_eq!(dce_module.enums.len(), 1);
    assert!(exists_enum(&dce_module, "Opt"));
}

#[test]
fn test_dce_enum_unused() {
    // enum Opt { Some(i32), None }; main() { 1 } -> keep main, remove Opt
    let main_sym = Symbol::new(1);
    let opt_sym = Symbol::new(10);
    let some_sym = Symbol::new(11);
    let none_sym = Symbol::new(12);
    let mut functions = BTreeMap::new();
    let mut enums = BTreeMap::new();

    enums.insert(opt_sym, TypedEnum { 
        symbol: opt_sym, 
        name: "Opt".to_string(), 
        variants: vec![
            TypedVariant { 
                name: "Some".to_string(), 
                symbol: some_sym, 
                fields: vec![TypedField {
                    name: "0".to_string(),
                    symbol: Symbol::new(100), // Using a new symbol for the field
                    ty: ty_i32(),
                    is_public: true,
                    span: dummy_span()
                }],
                span: dummy_span() 
            }, 
            TypedVariant { 
                name: "None".to_string(), 
                symbol: none_sym, 
                fields: vec![], // Unit variant has no fields
                span: dummy_span() 
            }
        ], 
        generic_params: vec![], 
        span: dummy_span() 
    });
    functions.insert(main_sym, TypedFunction { name: "main".to_string(), params: vec![], return_type: ty_i32(), body: Some(TypedExpr { kind: TypedExprKind::IntLiteral { value: 1, suffix: None }, ty: ty_i32(), span: dummy_span() }), generic_params: vec![], span: dummy_span(), is_effectful: false });

    let typed_module = create_typed_module_with_defs(functions, BTreeMap::new(), enums, Some(main_sym));
    let hir_module = lower_module_to_anf_hir(&typed_module);
    let dce_module = perform_dce(hir_module);

    assert_eq!(dce_module.functions.len(), 1);
    assert!(exists_fn(&dce_module, "main"));
    assert_eq!(dce_module.enums.len(), 0);
    assert!(!exists_enum(&dce_module, "Opt"));
}

#[test]
fn test_dce_type_reference() {
    // struct Point {}; fn process(p: Point) -> Point { p }; main() { let x = Point{}; process(x) } -> keep main, process, Point
    let main_sym = Symbol::new(1);
    let process_sym = Symbol::new(2);
    let point_sym = Symbol::new(10);
    let process_param_sym = Symbol::new(3);
    let main_var_x_sym = Symbol::new(4);
    let mut functions = BTreeMap::new();
    let mut structs = BTreeMap::new();
    
    structs.insert(point_sym, TypedStruct { 
        symbol: point_sym, 
        name: "Point".to_string(), 
        fields: vec![], 
        generic_params: vec![], 
        span: dummy_span() 
    });
    let process_params = vec![TypedParameter { 
        name: "p".to_string(),
        symbol: process_param_sym,
        ty: ty_named("Point"),
        is_variadic: false,
        has_default: false,
        span: dummy_span()
    }];
    functions.insert(process_sym, TypedFunction { 
        name: "process".to_string(), 
        params: process_params,
        return_type: ty_named("Point"), 
        body: Some(TypedExpr { kind: TypedExprKind::Variable { symbol: process_param_sym, name: "p".to_string() }, ty: ty_named("Point"), span: dummy_span() }), 
        generic_params: vec![], 
        span: dummy_span(), 
        is_effectful: false 
    });
    
    let point_constructor = TypedExpr { kind: TypedExprKind::Struct { 
        name: "Point".to_string(), 
        fields: vec![], 
        base: None,
        struct_symbol: point_sym,
    }, ty: ty_named("Point"), span: dummy_span() };
    let let_x = TypedExpr { kind: TypedExprKind::Let { pattern: TypedPattern { kind: TypedPatternKind::Identifier { symbol: main_var_x_sym, name: "x".to_string() }, ty: ty_named("Point"), span: dummy_span()}, value: Box::new(point_constructor)}, ty: ty_unit(), span: dummy_span()};
    let call_process = TypedExpr { kind: TypedExprKind::Call { func_expr: Box::new(TypedExpr { kind: TypedExprKind::Variable { symbol: process_sym, name: "process".to_string() }, ty: ty_fn(vec![ty_named("Point")], ty_named("Point")), span: dummy_span() }), func_symbol: Some(process_sym), type_args: None, args: vec![TypedArgument { name: None, value: TypedExpr { kind: TypedExprKind::Variable { symbol: main_var_x_sym, name: "x".to_string()}, ty: ty_named("Point"), span: dummy_span()}, span: dummy_span() }] }, ty: ty_named("Point"), span: dummy_span() };
    let main_body = TypedExpr { kind: TypedExprKind::Block(vec![let_x, call_process]), ty: ty_named("Point"), span: dummy_span() };
    functions.insert(main_sym, TypedFunction { name: "main".to_string(), params: vec![], return_type: ty_named("Point"), body: Some(main_body), generic_params: vec![], span: dummy_span(), is_effectful: false });

    let typed_module = create_typed_module_with_defs(functions, structs, BTreeMap::new(), Some(main_sym));
    let hir_module = lower_module_to_anf_hir(&typed_module);
    let dce_module = perform_dce(hir_module);

    // Expected: Keep main, process, Point
    assert_eq!(dce_module.functions.len(), 2);
    assert!(exists_fn(&dce_module, "main"));
    assert!(exists_fn(&dce_module, "process"));
    assert_eq!(dce_module.structs.len(), 1);
    assert!(exists_struct(&dce_module, "Point"));
}

#[test]
fn test_dce_recursive_function() {
    // main() { recurse(5) }; recurse(n) { recurse(0) } // Simplified HIR body
    let main_sym = Symbol::new(1);
    let recurse_sym = Symbol::new(2);
    let param_n_sym = Symbol::new(3);
    let mut functions = BTreeMap::new();

    let recurse_params = vec![TypedParameter { 
        name: "n".to_string(),
        symbol: param_n_sym,
        ty: ty_i32(),
        is_variadic: false,
        has_default: false,
        span: dummy_span() 
    }];
    let recurse_body_ast = TypedExpr { kind: TypedExprKind::IntLiteral { value: 0, suffix: None }, ty: ty_i32(), span: dummy_span() }; // Simplified AST body
    functions.insert(recurse_sym, TypedFunction { 
        name: "recurse".to_string(), 
        params: recurse_params,
        return_type: ty_i32(), 
        body: Some(recurse_body_ast), 
        generic_params: vec![], 
        span: dummy_span(), 
        is_effectful: false 
    });
    
    let main_body_ast = TypedExpr { kind: TypedExprKind::Call { 
        func_expr: Box::new(TypedExpr { kind: TypedExprKind::Variable { symbol: recurse_sym, name: "recurse".to_string() }, ty: ty_fn(vec![ty_i32()], ty_i32()), span: dummy_span() }), 
        func_symbol: Some(recurse_sym), 
        type_args: None, 
        args: vec![TypedArgument { name: None, value: TypedExpr { kind: TypedExprKind::IntLiteral { value: 5, suffix: None }, ty: ty_i32(), span: dummy_span() }, span: dummy_span() }] 
    }, ty: ty_i32(), span: dummy_span() };
    functions.insert(main_sym, TypedFunction { name: "main".to_string(), params: vec![], return_type: ty_i32(), body: Some(main_body_ast), generic_params: vec![], span: dummy_span(), is_effectful: false });

    let typed_module = create_typed_module(functions, Some(main_sym));
    let hir_module = lower_module_to_anf_hir(&typed_module);
    
    // Add the recursive call back into HIR manually for testing DCE graph traversal
    let mut hir_module_mut = hir_module;
    let recurse_fn_hir = hir_module_mut.functions.iter_mut().find(|f| f.symbol == recurse_sym).unwrap();
    let recurse_call_val = HirValue::Call { func: Operand::Global(recurse_sym), args: vec![Operand::Const(HirLiteral::IntLiteral { value: 0, ty: PrimitiveType::I32 })] };
    // Need a unique HirVar ID. Get max from initial lowering + offset.
    let next_var_id = hir_module_mut.next_var_id; 
    let binding_var = HirVar(next_var_id); 
    hir_module_mut.next_var_id += 1; // Increment for next potential use
    
    // Define the final tail return expression first
    let tail_return = HirExpr { 
        kind: HirExprKind::Tail(HirTailExpr::Value(Operand::Var(binding_var))), // Return the call result
        ty: HirType::Primitive(parallax_hir::PrimitiveType::I32), 
        span: dummy_span() 
    };
    
    // Define the Let expression binding the recursive call
    let recurse_call_expr = HirExpr { 
        kind: HirExprKind::Let { 
            var: binding_var, 
            var_ty: HirType::Primitive(parallax_hir::PrimitiveType::I32), 
            value: Box::new(recurse_call_val), 
            rest: Box::new(tail_return) // Use the defined tail return
        },
        ty: HirType::Primitive(parallax_hir::PrimitiveType::I32), 
        span: dummy_span()
    };
    
    recurse_fn_hir.body = Some(recurse_call_expr);

    let dce_module = perform_dce(hir_module_mut);

    // Expected: Keep main and recurse
    assert_eq!(dce_module.functions.len(), 2);
    assert!(exists_fn(&dce_module, "main"));
    assert!(exists_fn(&dce_module, "recurse"));
}

#[test]
fn test_dce_mutually_recursive() {
    // main->A; A->B; B->A. Keep main, A, B.
    let main_sym = Symbol::new(1);
    let a_sym = Symbol::new(2);
    let b_sym = Symbol::new(3);
    let mut functions = BTreeMap::new();
    let fn_type = ty_fn(vec![], ty_i32());

    // Need bodies for HIR modification
    let body_a = TypedExpr { kind: TypedExprKind::Call { 
        func_expr: Box::new(TypedExpr { kind: TypedExprKind::Variable { symbol: b_sym, name: "B".to_string() }, ty: fn_type.clone(), span: dummy_span() }), 
        func_symbol: Some(b_sym), 
        type_args: None, 
        args: vec![] 
    }, ty: ty_i32(), span: dummy_span() };
    let body_b = TypedExpr { kind: TypedExprKind::Call { 
        func_expr: Box::new(TypedExpr { kind: TypedExprKind::Variable { symbol: a_sym, name: "A".to_string() }, ty: fn_type.clone(), span: dummy_span() }), 
        func_symbol: Some(a_sym), 
        type_args: None, 
        args: vec![] 
    }, ty: ty_i32(), span: dummy_span() };
    let body_main = TypedExpr { kind: TypedExprKind::Call { 
        func_expr: Box::new(TypedExpr { kind: TypedExprKind::Variable { symbol: a_sym, name: "A".to_string() }, ty: fn_type.clone(), span: dummy_span() }), 
        func_symbol: Some(a_sym), 
        type_args: None, 
        args: vec![] 
    }, ty: ty_i32(), span: dummy_span() };

    functions.insert(a_sym, TypedFunction { name: "A".to_string(), params: vec![], return_type: ty_i32(), body: Some(body_a), generic_params: vec![], span: dummy_span(), is_effectful: false });
    functions.insert(b_sym, TypedFunction { name: "B".to_string(), params: vec![], return_type: ty_i32(), body: Some(body_b), generic_params: vec![], span: dummy_span(), is_effectful: false });
    functions.insert(main_sym, TypedFunction { name: "main".to_string(), params: vec![], return_type: ty_i32(), body: Some(body_main), generic_params: vec![], span: dummy_span(), is_effectful: false });

    let typed_module = create_typed_module(functions, Some(main_sym));
    let hir_module = lower_module_to_anf_hir(&typed_module);
    // No need to modify HIR for mutual recursion test, lowering handles calls.
    let dce_module = perform_dce(hir_module);

    assert_eq!(dce_module.functions.len(), 3);
    assert!(exists_fn(&dce_module, "main"));
    assert!(exists_fn(&dce_module, "A"));
    assert!(exists_fn(&dce_module, "B"));
}

#[test]
fn test_dce_lambda_no_capture() {
    // main() { let f = |x| -> i32 { 99 }; f(1) } -> keep main and lambda_fn
    let main_sym = Symbol::new(1);
    let var_sym_f = Symbol::new(2);
    let param_sym_x = Symbol::new(3);
    let mut functions = BTreeMap::new();
    let ty_i32 = ty_i32();
    let ty_fn_i32_to_i32 = ty_fn(vec![ty_i32.clone()], ty_i32.clone());

    let lambda_params = vec![TypedParameter {
        name: "x".to_string(),
        symbol: param_sym_x,
        ty: ty_i32.clone(),
        is_variadic: false,
        has_default: false,
        span: dummy_span()
    }];

    let let_f = TypedExpr { kind: TypedExprKind::Let { 
        pattern: TypedPattern { kind: TypedPatternKind::Identifier { symbol: var_sym_f, name: "f".to_string() }, ty: ty_fn_i32_to_i32.clone(), span: dummy_span() }, 
        value: Box::new(TypedExpr { kind: TypedExprKind::Lambda { 
            params: lambda_params,
            body: Box::new(TypedExpr { kind: TypedExprKind::IntLiteral { value: 99, suffix: None }, ty: ty_i32.clone(), span: dummy_span() }) 
        }, ty: ty_fn_i32_to_i32.clone(), span: dummy_span() }) 
    }, ty: ty_unit(), span: dummy_span() };
    let call_f = TypedExpr { kind: TypedExprKind::Call { func_expr: Box::new(TypedExpr { kind: TypedExprKind::Variable { symbol: var_sym_f, name: "f".to_string() }, ty: ty_fn_i32_to_i32.clone(), span: dummy_span() }), func_symbol: Some(var_sym_f), type_args: None, args: vec![TypedArgument { name: None, value: TypedExpr { kind: TypedExprKind::IntLiteral { value: 1, suffix: None }, ty: ty_i32.clone(), span: dummy_span() }, span: dummy_span() }] }, ty: ty_i32.clone(), span: dummy_span() };
    let block_expr = TypedExpr { kind: TypedExprKind::Block(vec![let_f, call_f]), ty: ty_i32.clone(), span: dummy_span() };
    functions.insert(main_sym, TypedFunction { name: "main".to_string(), params: vec![], return_type: ty_i32.clone(), body: Some(block_expr), generic_params: vec![], span: dummy_span(), is_effectful: false });

    let typed_module = create_typed_module(functions, Some(main_sym));
    let hir_module = lower_module_to_anf_hir(&typed_module);
    // Lowering creates 2 functions (main + lambda impl)
    assert_eq!(hir_module.functions.len(), 2);
    let lambda_fn_name = hir_module.functions.iter().find(|f| f.symbol != main_sym).unwrap().name.clone();

    let dce_module = perform_dce(hir_module);

    // DCE should keep both main and the generated lambda function
    assert_eq!(dce_module.functions.len(), 2);
    assert!(exists_fn(&dce_module, "main"));
    assert!(exists_fn(&dce_module, &lambda_fn_name));
}

#[test]
fn test_dce_lambda_with_capture() {
    // main() { let y = 10; let f = |x| -> i32 { x + y }; f(1) } -> keep main and lambda_fn
    let main_sym = Symbol::new(1);
    let var_sym_y = Symbol::new(2);
    let var_sym_f = Symbol::new(3);
    let param_sym_x = Symbol::new(4);
    let add_intrinsic_sym = Symbol::new(100); // Simplified: Assume `add` exists
    let mut functions = BTreeMap::new();

    let ty_i32 = ty_i32();
    let ty_fn_i32_to_i32 = ty_fn(vec![ty_i32.clone()], ty_i32.clone());
    let ty_fn_i32_i32_to_i32 = ty_fn(vec![ty_i32.clone(), ty_i32.clone()], ty_i32.clone());

    let let_y = TypedExpr { kind: TypedExprKind::Let { pattern: TypedPattern { kind: TypedPatternKind::Identifier { symbol: var_sym_y, name: "y".to_string() }, ty: ty_i32.clone(), span: dummy_span() }, value: Box::new(TypedExpr { kind: TypedExprKind::IntLiteral { value: 10, suffix: None }, ty: ty_i32.clone(), span: dummy_span() }) }, ty: ty_unit(), span: dummy_span() };
    let lambda_body_expr = TypedExpr { kind: TypedExprKind::Call { 
        func_expr: Box::new(TypedExpr { kind: TypedExprKind::Variable { symbol: add_intrinsic_sym, name: "add".to_string() }, ty: ty_fn_i32_i32_to_i32.clone(), span: dummy_span() }), 
        func_symbol: Some(add_intrinsic_sym), 
        type_args: None,
        args: vec![ 
            TypedArgument { name: None, value: TypedExpr { kind: TypedExprKind::Variable { symbol: param_sym_x, name: "x".to_string() }, ty: ty_i32.clone(), span: dummy_span() }, span: dummy_span() }, 
            TypedArgument { name: None, value: TypedExpr { kind: TypedExprKind::Variable { symbol: var_sym_y, name: "y".to_string() }, ty: ty_i32.clone(), span: dummy_span() }, span: dummy_span() }
        ] 
    }, ty: ty_i32.clone(), span: dummy_span() };
    let let_f = TypedExpr { kind: TypedExprKind::Let { 
        pattern: TypedPattern { kind: TypedPatternKind::Identifier { symbol: var_sym_f, name: "f".to_string() }, ty: ty_fn_i32_to_i32.clone(), span: dummy_span() }, 
        value: Box::new(TypedExpr { kind: TypedExprKind::Lambda { 
            params: vec![TypedParameter {
                name: "x".to_string(),
                symbol: param_sym_x,
                ty: ty_i32.clone(),
                is_variadic: false,
                has_default: false,
                span: dummy_span()
            }],
            body: Box::new(lambda_body_expr) 
        }, ty: ty_fn_i32_to_i32.clone(), span: dummy_span() }) 
    }, ty: ty_unit(), span: dummy_span() };
    let call_f = TypedExpr { kind: TypedExprKind::Call { func_expr: Box::new(TypedExpr { kind: TypedExprKind::Variable { symbol: var_sym_f, name: "f".to_string() }, ty: ty_fn_i32_to_i32.clone(), span: dummy_span() }), func_symbol: Some(var_sym_f), type_args: None, args: vec![TypedArgument { name: None, value: TypedExpr { kind: TypedExprKind::IntLiteral { value: 1, suffix: None }, ty: ty_i32.clone(), span: dummy_span() }, span: dummy_span() }] }, ty: ty_i32.clone(), span: dummy_span() };
    let block_expr = TypedExpr { kind: TypedExprKind::Block(vec![let_y, let_f, call_f]), ty: ty_i32.clone(), span: dummy_span() };
    functions.insert(main_sym, TypedFunction { name: "main".to_string(), params: vec![], return_type: ty_i32.clone(), body: Some(block_expr), generic_params: vec![], span: dummy_span(), is_effectful: false });

    let typed_module = create_typed_module(functions, Some(main_sym));
    let hir_module = lower_module_to_anf_hir(&typed_module);
    assert_eq!(hir_module.functions.len(), 2);
    let lambda_fn_name = hir_module.functions.iter().find(|f| f.symbol != main_sym).unwrap().name.clone();

    let dce_module = perform_dce(hir_module);

    // DCE should keep both main and the generated lambda function (captures handled internally)
    assert_eq!(dce_module.functions.len(), 2);
    assert!(exists_fn(&dce_module, "main"));
    assert!(exists_fn(&dce_module, &lambda_fn_name));
}