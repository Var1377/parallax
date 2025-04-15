
use parallax_syntax::{ParallaxParser, SyntaxError};
use parallax_syntax::ast::expr::ExprKind;
use parallax_syntax::ast::items::*;
use parallax_syntax::ast::pattern::*;

// Helper to parse source and check for errors
// Updated to handle (Vec<Item>, Vec<SyntaxError>) return type
fn parse_source_expect_ok(source: &str) -> Vec<Item> {
    let mut parser = ParallaxParser::new().expect("Failed to create parser");
    let (items, errors) = parser.parse_ast(source);
    if !errors.is_empty() {
        panic!("Expected successful parse, but got errors: {:?}", errors);
    }
    if items.is_empty() && !source.trim().is_empty() {
        // If the source wasn't empty, we should have parsed *something*
        panic!("Parsing succeeded but produced no items for non-empty source.");
    }
    items
}

fn parse_source_expect_error(source: &str) -> Vec<SyntaxError> {
    let mut parser = ParallaxParser::new().expect("Failed to create parser");
    let (items, errors) = parser.parse_ast(source);
    if errors.is_empty() {
        panic!("Expected parsing errors, but none were found. Parsed items: {:?}", items);
    }
    errors
}

#[test]
fn test_complex_module_structure() {
    const SOURCE: &str = include_str!("complex_module.plx");
    let items = parse_source_expect_ok(SOURCE);

    assert_eq!(items.len(), 1, "Expected one top-level item (the outer module)");

    let outer_mod_item = &items[0];
    assert!(outer_mod_item.visibility, "Outer module should be public");

    if let ItemKind::Module(outer_mod) = &outer_mod_item.kind {
        assert_eq!(outer_mod.name.name, "outer");
        assert_eq!(outer_mod.items.len(), 8, "Expected 8 items inside outer module");

        // 1. Use statements
        let use_map_hashset = &outer_mod.items[0];
        if let ItemKind::Use(use_decl) = &use_map_hashset.kind {
            if let UseTreeKind::Path { segment, sub_tree, .. } = &use_decl.tree.kind {
                assert_eq!(segment.name, "std");
                if let Some(sub) = sub_tree {
                    if let UseTreeKind::Path { segment, sub_tree, .. } = &sub.kind {
                        assert_eq!(segment.name, "collections");
                            if let Some(group_tree) = sub_tree {
                            if let UseTreeKind::Group(group) = &group_tree.kind {
                                assert_eq!(group.len(), 2);
                                // HashMap as Map
                                if let UseTreeKind::Path{ segment, alias, sub_tree: st } = &group[0].kind {
                                    assert_eq!(segment.name, "HashMap");
                                    assert_eq!(alias.as_ref().unwrap().name, "Map");
                                    assert!(st.is_none());
                                } else { panic!("Expected Path in group") }
                                // HashSet
                                    if let UseTreeKind::Path{ segment, alias, sub_tree: st } = &group[1].kind {
                                    assert_eq!(segment.name, "HashSet");
                                    assert!(alias.is_none());
                                    assert!(st.is_none());
                                } else { panic!("Expected Path in group") }
                            } else { panic!("Expected Group") }
                        } else { panic!("Expected sub_tree for collections") }
                    } else { panic!("Expected Path for collections") }
                } else { panic!("Expected sub_tree for std") }
            } else { panic!("Expected Path for std") }
        } else { panic!("Expected Use item") }

        let use_glob = &outer_mod.items[1];
            if let ItemKind::Use(use_decl) = &use_glob.kind {
                if let UseTreeKind::Path { segment, sub_tree, .. } = &use_decl.tree.kind {
                    assert_eq!(segment.name, "super");
                    if let Some(sub) = sub_tree {
                        if let UseTreeKind::Path { segment, sub_tree, .. } = &sub.kind {
                            assert_eq!(segment.name, "utils");
                            if let Some(glob_tree) = sub_tree {
                            assert!(matches!(glob_tree.kind, UseTreeKind::Glob));
                            } else { panic!("Expected sub_tree for utils") }
                        } else { panic!("Expected Path for utils") }
                    } else { panic!("Expected sub_tree for super") }
                } else { panic!("Expected Path for super") }
            } else { panic!("Expected Use item") }

        // 2. Trait definition (Processor)
        let trait_item = &outer_mod.items[2];
        assert!(trait_item.visibility, "Processor trait should be public");
        if let ItemKind::Trait(trait_def) = &trait_item.kind {
            assert_eq!(trait_def.name.name, "Processor");
            assert_eq!(trait_def.generic_params.as_ref().unwrap().len(), 1);
            assert_eq!(trait_def.generic_params.as_ref().unwrap()[0].name.name, "I");
            assert!(trait_def.where_clause.is_none());
            assert_eq!(trait_def.items.len(), 2);
            // Associated type
            if let TraitItem::AssociatedType { name, .. } = &trait_def.items[0] {
                assert_eq!(name.name, "Output");
            } else { panic!("Expected AssociatedType") }
            // Method signature
            if let TraitItem::Method { function, .. } = &trait_def.items[1] {
                assert_eq!(function.name.name, "process");
                assert_eq!(function.params.len(), 2);
                assert!(function.body.is_none()); // Signature only
            } else { panic!("Expected Method") }
        } else { panic!("Expected Trait item") }

        // 3. Struct definition (DataWrapper)
        let struct_item = &outer_mod.items[3];
        assert!(!struct_item.visibility);
            if let ItemKind::Struct(struct_def) = &struct_item.kind {
                assert_eq!(struct_def.name.name, "DataWrapper");
                assert_eq!(struct_def.generic_params.as_ref().unwrap().len(), 1);
                assert!(struct_def.where_clause.is_some());
                assert_eq!(struct_def.fields.len(), 2);
                assert_eq!(struct_def.fields[0].name.name, "inner");
                assert_eq!(struct_def.fields[1].name.name, "id");
            } else { panic!("Expected Struct item") }

        // 4. Enum definition (Status)
        let enum_item = &outer_mod.items[4];
            assert!(!enum_item.visibility);
            if let ItemKind::Enum(enum_def) = &enum_item.kind {
                assert_eq!(enum_def.name.name, "Status");
                assert_eq!(enum_def.generic_params.as_ref().unwrap().len(), 2);
                assert!(enum_def.where_clause.is_none());
                assert_eq!(enum_def.variants.len(), 3);
                assert!(matches!(enum_def.variants[0].kind, EnumVariantKind::Tuple(_)));
                assert!(matches!(enum_def.variants[1].kind, EnumVariantKind::Struct(_)));
                assert!(matches!(enum_def.variants[2].kind, EnumVariantKind::Unit));
            } else { panic!("Expected Enum item") }

        // 5. Inner module definition
        let inner_mod_item = &outer_mod.items[5];
        // assert!(inner_mod_item.visibility); // pub(crate) needs specific check if AST supports it
            if let ItemKind::Module(inner_mod) = &inner_mod_item.kind {
                assert_eq!(inner_mod.name.name, "inner");
                assert_eq!(inner_mod.items.len(), 2); // Use + Function
                // Check use statement path
                if let ItemKind::Use(use_decl) = &inner_mod.items[0].kind {
                    if let UseTreeKind::Path { segment, sub_tree, .. } = &use_decl.tree.kind {
                    assert_eq!(segment.name, "super");
                        if let Some(sub) = sub_tree {
                        if let UseTreeKind::Path { segment, sub_tree, .. } = &sub.kind {
                            assert_eq!(segment.name, "super");
                                if let Some(sub2) = sub_tree {
                                    if let UseTreeKind::Path { segment, sub_tree, .. } = &sub2.kind {
                                    assert_eq!(segment.name, "config");
                                        if let Some(sub3) = sub_tree {
                                            if let UseTreeKind::Path { segment, sub_tree, .. } = &sub3.kind {
                                            assert_eq!(segment.name, "Settings");
                                                assert!(sub_tree.is_none());
                                            } else {panic!("Expected Path Settings")} 
                                        } else {panic!("Expected sub_tree for Settings")} 
                                    } else {panic!("Expected Path config")} 
                                } else {panic!("Expected sub_tree for config")} 
                        } else {panic!("Expected Path super2")} 
                        } else {panic!("Expected sub_tree for super2")} 
                    } else {panic!("Expected Path super1")} 
                } else { panic!("Expected Use item") }
            } else { panic!("Expected inner module") }

        // 6. Impl definition
        let impl_item = &outer_mod.items[6];
        if let ItemKind::Impl(impl_def) = &impl_item.kind {
                assert_eq!(impl_def.generic_params.as_ref().unwrap().len(), 1);
                assert!(impl_def.trait_type.is_some());
                assert!(impl_def.self_type.is_kind_app()); // Check it's DataWrapper<T>
                assert!(impl_def.where_clause.is_some()); // From trait bound? Parser needs update or adjust test
                assert_eq!(impl_def.items.len(), 2); // Associated Type + Method
                assert!(matches!(impl_def.items[0], ImplItem::AssociatedType { .. }));
                assert!(matches!(impl_def.items[1], ImplItem::Method(_)));
                if let ImplItem::Method(func) = &impl_def.items[1] {
                    assert!(func.body.is_some()); // Check it has an implementation
                }
            } else { panic!("Expected Impl item") }

        // 7. Function definition (run_processor)
        let func_item = &outer_mod.items[7];
            assert!(func_item.visibility);
            if let ItemKind::Function(func_def) = &func_item.kind {
            assert_eq!(func_def.name.name, "run_processor");
            assert_eq!(func_def.generic_params.as_ref().unwrap().len(), 2);
            assert_eq!(func_def.params.len(), 2);
            assert!(func_def.return_type.is_some());
            assert!(func_def.where_clause.is_some());
            assert!(func_def.body.is_some());
            } else { panic!("Expected Function item") }

    } else {
        panic!("Expected top-level item to be a Module, got {:?}", outer_mod_item.kind);
    }
}

#[test]
fn test_advanced_types_and_functions() {
    const SOURCE: &str = include_str!("advanced_types.plx");
    let items = parse_source_expect_ok(SOURCE);
    assert_eq!(items.len(), 5, "Expected 5 top-level items");

    // 1. Config struct
    let config_item = &items[0];
        if let ItemKind::Struct(struct_def) = &config_item.kind {
        assert_eq!(struct_def.name.name, "Config");
        assert_eq!(struct_def.generic_params.as_ref().unwrap().len(), 2);
        // TODO: Check default value for generic param B if AST supports it
        assert_eq!(struct_def.fields.len(), 3);
        assert!(struct_def.fields[0].visibility); // setting_a
        assert!(struct_def.fields[1].visibility); // setting_b
        assert!(!struct_def.fields[2].visibility); // internal
        } else { panic!("Expected Config struct")}

    // 2. Message enum
    let msg_item = &items[1];
    if let ItemKind::Enum(enum_def) = &msg_item.kind {
        assert_eq!(enum_def.name.name, "Message");
        assert_eq!(enum_def.generic_params.as_ref().unwrap().len(), 1);
        assert_eq!(enum_def.variants.len(), 3);
        // Data variant (Struct)
        assert_eq!(enum_def.variants[0].name.name, "Data");
        if let EnumVariantKind::Struct(fields) = &enum_def.variants[0].kind {
            assert_eq!(fields.len(), 3);
            assert_eq!(fields[0].name.name, "id");
            assert_eq!(fields[1].name.name, "payload");
            assert_eq!(fields[2].name.name, "metadata");
        } else { panic!("Expected Struct variant Kind") }
        // Control variant (Tuple)
        assert_eq!(enum_def.variants[1].name.name, "Control");
        assert!(matches!(enum_def.variants[1].kind, EnumVariantKind::Tuple(_)));
        // Signal variant (Unit)
        assert_eq!(enum_def.variants[2].name.name, "Signal");
        assert!(matches!(enum_def.variants[2].kind, EnumVariantKind::Unit));
    } else { panic!("Expected Message enum")}

    // 3. ControlOp enum
    let op_item = &items[2];
    if let ItemKind::Enum(enum_def) = &op_item.kind {
        assert_eq!(enum_def.name.name, "ControlOp");
        assert!(enum_def.generic_params.is_none());
        assert_eq!(enum_def.variants.len(), 3);
        assert!(matches!(enum_def.variants[0].kind, EnumVariantKind::Unit)); // Start
        assert!(matches!(enum_def.variants[1].kind, EnumVariantKind::Unit)); // Stop
        assert!(matches!(enum_def.variants[2].kind, EnumVariantKind::Struct(_))); // Restart
    } else { panic!("Expected ControlOp enum")}

    // 4. handle_message function
    let handle_fn_item = &items[3];
    if let ItemKind::Function(func) = &handle_fn_item.kind {
        assert_eq!(func.name.name, "handle_message");
        assert_eq!(func.generic_params.as_ref().unwrap().len(), 1);
        assert!(func.where_clause.is_some());
        assert_eq!(func.params.len(), 2);
        // Check body has match expression
        assert!(func.body.is_some());
        if let Some(body_expr) = &func.body {
            if let ExprKind::Block(block_items) = &body_expr.kind {
                // Find the match expression (might be wrapped in block)
                let match_expr = block_items.iter().find_map(|expr| {
                    if let ExprKind::Match { .. } = expr.kind { Some(expr) } else { None }
                }).expect("Could not find match expression in function body");

                if let ExprKind::Match { arms, .. } = &match_expr.kind {
                        assert_eq!(arms.len(), 3);
                        // Check first arm pattern (Struct with ..)
                        let (pattern1, _) = &arms[0];
                        if let PatternKind::ConstructorWithArgs { path, args } = &pattern1.kind {
                            assert_eq!(path.last().unwrap().name, "Data");
                            if let PatternKind::Struct { fields, .. } = &args.kind {
                                assert_eq!(fields.len(), 2); // id, payload explicitly named
                                // TODO: Need AST support to verify '..' presence specifically
                            } else { panic!("Expected Struct pattern args") }
                        } else { panic!("Expected Constructor pattern") }
                } else { panic!("Expected Match expr kind") }
            } else { panic!("Expected Block body") }
        } else {panic!("Expected function body"); }
    } else { panic!("Expected handle_message function")}

    // 5. configure function
    let config_fn_item = &items[4];
    if let ItemKind::Function(func) = &config_fn_item.kind {
            assert_eq!(func.name.name, "configure");
            assert!(func.generic_params.is_none());
            assert_eq!(func.params.len(), 3);
            // timeout param with default value
            assert!(func.params[1].default_value.is_some());
    } else { panic!("Expected configure function")}
}

#[test]
fn test_invalid_syntax_handling() {
    const SOURCE: &str = include_str!("invalid_syntax.plx");
    let errors = parse_source_expect_error(SOURCE);

    println!("Successfully caught parse errors: {:?}", errors);
    assert!(!errors.is_empty(), "Expected parsing errors");

    // Now we can assert more specifically about the errors if needed
    // assert!(errors.iter().any(|e| format!("{:?}", e).contains("expected ')'")));
}

#[test]
fn test_expressions() {
    const SOURCE: &str = include_str!("expressions.plx");
    let items = parse_source_expect_ok(SOURCE);
    assert_eq!(items.len(), 6);

    let operators_fn = find_function(&items, "operators").expect("operators function not found");
        assert!(operators_fn.body.is_some());
        // TODO: Traverse the body (likely a block) and assert specific expression kinds
        // Example: find 'let a = ...' and assert it's a BinaryExpr with correct precedence applied
        // find 'let l = |...|' and assert it's a Lambda expr
        // find 'let x = match ...' and assert it's a Match expr

    let complex_call_fn = find_function(&items, "complex_call").expect("complex_call function not found");
        assert!(complex_call_fn.body.is_some());
        // TODO: Find the call expression and assert its structure:
        // - Named arguments (Argument struct has name: Some(Ident))
        // - Struct literal expression used as an argument value
}

#[test]
fn test_patterns() {
    const SOURCE: &str = include_str!("patterns.plx");
    let items = parse_source_expect_ok(SOURCE);
    assert_eq!(items.len(), 3);

    let match_fn = find_function(&items, "match_test").expect("match_test function not found");
        assert!(match_fn.body.is_some());
        // TODO: Find the match expression and assert arm patterns:
        // - Some((x, Point { y, .. })) => check Constructor, Tuple, Struct pattern with '..'
        // - Some((0 | 1, _)) => check Or pattern, Literal, Wildcard
        // - Some((z @ 2..=10, p)) => check Binding, Range pattern (if AST supports), Tuple
        // - Need support for match guards (if x > y)

    let let_fn = find_function(&items, "let_patterns").expect("let_patterns function not found");
        assert!(let_fn.body.is_some());
        // TODO: Find let statements and assert patterns:
        // - Point { x, y: y_val } => Struct pattern with rename
        // - (first, .., last) => Tuple pattern with rest ('..')
        // - [a, b, ..] => Array pattern with rest ('..')
        // - Ok(value) => Constructor pattern
}

// Helper to find a function item by name
fn find_function<'a>(items: &'a [Item], name: &str) -> Option<&'a Function> {
    items.iter().find_map(|item| {
        if let ItemKind::Function(func) = &item.kind {
            if func.name.name == name {
                return Some(func);
            }
        }
        None
    })
}

// TODO: Add more tests covering:
// - Literals: different bases (hex, oct, bin), suffixes, raw strings, byte strings, escapes
// - Paths: ::mod, crate::, super:: in various contexts
// - Complex generic bounds and where clauses in various items
// - Associated types in impls