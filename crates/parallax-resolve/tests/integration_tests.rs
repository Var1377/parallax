use parallax_resolve::{
    ResolveDatabase, ResolutionError,
    ResolvedModuleStructure, ResolvedType, PrimitiveType
};
use parallax_syntax::{SyntaxDatabase, ModuleUnit, ModuleOriginKind, ParsedFile, ast};
use parallax_source::{SourceDatabase, SourceFile};

// --- Mock Database Setup ---
// A simple in-memory database for testing.
#[salsa::db]
#[derive(Default, Clone)]
struct TestDb {
    storage: salsa::Storage<Self>,
}

// Implement the required traits for our TestDb
impl salsa::Database for TestDb {
    fn salsa_event(&self, event: &dyn Fn() -> salsa::Event) {
        // Just execute the event function but don't do anything with result
        let _ = event();
    }
}

#[salsa::db]
impl SourceDatabase for TestDb {}

#[salsa::db]
impl SyntaxDatabase for TestDb {}

#[salsa::db]
impl ResolveDatabase for TestDb {}

// --- Helper Functions for Testing ---

// This is a tracked function in our database trait
// We need this since Salsa tracked objects must be created in a tracked context
#[salsa::tracked]
fn test_create_module_unit<'db>(
    db: &'db dyn SyntaxDatabase,
    name: String,
    content: String,
    items: Vec<ast::items::Item>,
) -> ModuleUnit<'db> {
    println!("DEBUG: Creating module unit '{}' with {} items", name, items.len());
    
    // Create source file and parsed file
    let source_file = SourceFile::new(db, format!("{}.plx", name), content);
    let parsed_file = ParsedFile::new(db, items, Vec::new());
    
    // Create and return module unit
    ModuleUnit::new(
        db,
        name.clone(),
        format!("test::{}", name),
        Some(source_file),
        Some(parsed_file),
        ModuleOriginKind::File,
        Vec::new()  // No child modules for simplicity
    )
}

// Parses source text by using parallax_syntax's parser
// For testing, we'll use a simplified mock parser
fn parse_source(source: &str) -> Vec<ast::items::Item> {
    println!("DEBUG: Parsing source: {}", source);
    // This is a mock function that would normally use the real parser
    // For testing, we can implement a simplified parser or return pre-constructed AST nodes
    
    // Example implementation - a very minimal mock parser
    // In real tests, you'd use tree-sitter or another parser, or pre-construct AST nodes
    // based on test requirements
    
    // Note: This is just a placeholder. For real tests, you'd want to use 
    // the actual parser or create specific AST nodes directly
    if source.is_empty() {
        println!("DEBUG: Empty source, returning empty item list");
        return Vec::new();
    }

    // This is a fake parser implementation for test_simple_struct
    // In reality, you'd either call the real parser or construct AST nodes manually
    // based on what each test needs
    if source.contains("struct Point") {
        println!("DEBUG: Creating AST for 'struct Point'");
        // Create a fake struct AST node for test_simple_struct
        use parallax_syntax::ast::common::Ident;
        use parallax_syntax::ast::items::{Item, ItemKind, StructDef, StructField};
        use parallax_syntax::ast::types::{Type, TypeKind};
        use miette::SourceSpan;
        
        // Use 0 as usize instead of i32
        let dummy_span = || SourceSpan::new(0_usize.into(), 0_usize.into());
        
        // Create x field
        let x_field = StructField {
            name: Ident { name: "x".to_string(), span: dummy_span() },
            ty: Type::new(TypeKind::Path(vec![Ident { name: "i32".to_string(), span: dummy_span() }]), dummy_span()),
            visibility: true,
            span: dummy_span(),
        };
        
        // Create y field
        let y_field = StructField {
            name: Ident { name: "y".to_string(), span: dummy_span() },
            ty: Type::new(TypeKind::Path(vec![Ident { name: "i32".to_string(), span: dummy_span() }]), dummy_span()),
            visibility: true,
            span: dummy_span(),
        };
        
        // Create struct
        let struct_def = StructDef {
            name: Ident { name: "Point".to_string(), span: dummy_span() },
            generic_params: None,
            where_clause: None,
            fields: vec![x_field, y_field],
            span: dummy_span(),
        };
        
        // Create item
        let item = Item {
            kind: ItemKind::Struct(struct_def),
            visibility: true, 
            span: dummy_span(),
        };
        
        println!("DEBUG: Created struct Point AST with 2 fields");
        return vec![item];
    }
    
    // This is a fake parser implementation for test_simple_function_signature
    if source.contains("fn add") {
        println!("DEBUG: Creating AST for 'fn add'");
        // Create a fake function AST node for test_simple_function_signature
        use parallax_syntax::ast::common::Ident;
        use parallax_syntax::ast::items::{Item, ItemKind, Function, Parameter};
        use parallax_syntax::ast::types::{Type, TypeKind};
        use parallax_syntax::ast::pattern::{Pattern, PatternKind};
        use miette::SourceSpan;
        
        // Use 0 as usize instead of i32
        let dummy_span = || SourceSpan::new(0_usize.into(), 0_usize.into());
        
        // Create parameter a
        let param_a = Parameter {
            pattern: Pattern::new(PatternKind::Identifier(Ident { name: "a".to_string(), span: dummy_span() }), dummy_span()),
            ty: Some(Type::new(TypeKind::Path(vec![Ident { name: "f32".to_string(), span: dummy_span() }]), dummy_span())),
            default_value: None,
            is_variadic: false,
            span: dummy_span(),
        };
        
        // Create parameter b
        let param_b = Parameter {
            pattern: Pattern::new(PatternKind::Identifier(Ident { name: "b".to_string(), span: dummy_span() }), dummy_span()),
            ty: Some(Type::new(TypeKind::Path(vec![Ident { name: "f32".to_string(), span: dummy_span() }]), dummy_span())),
            default_value: None,
            is_variadic: false,
            span: dummy_span(),
        };
        
        // Create return type
        let return_type = Type::new(TypeKind::Path(vec![Ident { name: "f32".to_string(), span: dummy_span() }]), dummy_span());
        
        // Create function
        let function = Function {
            name: Ident { name: "add".to_string(), span: dummy_span() },
            generic_params: None,
            params: vec![param_a, param_b],
            return_type: Some(return_type),
            where_clause: None,
            body: None, // No body for this test
            span: dummy_span(),
        };
        
        // Create item
        let item = Item {
            kind: ItemKind::Function(function),
            visibility: true,
            span: dummy_span(),
        };
        
        println!("DEBUG: Created fn add AST with 2 parameters");
        return vec![item];
    }
    
    // This is a fake parser implementation for test_duplicate_definition_error
    if source.contains("fn foo") && source.contains("struct foo") {
        println!("DEBUG: Creating AST for duplicate 'foo' definitions");
        // Create a fake function and struct AST nodes for test_duplicate_definition_error
        use parallax_syntax::ast::common::Ident;
        use parallax_syntax::ast::items::{Item, ItemKind, Function, StructDef, StructField};
        use parallax_syntax::ast::types::{Type, TypeKind};
        use miette::SourceSpan;
        
        // Use 0 as usize instead of i32
        let dummy_span = || SourceSpan::new(0_usize.into(), 0_usize.into());
        
        // Create function
        let function = Function {
            name: Ident { name: "foo".to_string(), span: dummy_span() },
            generic_params: None,
            params: Vec::new(),
            return_type: Some(Type::new(TypeKind::Tuple(Vec::new()), dummy_span())),
            where_clause: None,
            body: None,
            span: dummy_span(),
        };
        
        // Create struct
        let x_field = StructField {
            name: Ident { name: "x".to_string(), span: dummy_span() },
            ty: Type::new(TypeKind::Path(vec![Ident { name: "f32".to_string(), span: dummy_span() }]), dummy_span()),
            visibility: true,
            span: dummy_span(),
        };
        
        let struct_def = StructDef {
            name: Ident { name: "foo".to_string(), span: dummy_span() },
            generic_params: None,
            where_clause: None,
            fields: vec![x_field],
            span: dummy_span(),
        };
        
        // Create items
        let func_item = Item {
            kind: ItemKind::Function(function),
            visibility: true,
            span: dummy_span(),
        };
        
        let struct_item = Item {
            kind: ItemKind::Struct(struct_def),
            visibility: true,
            span: dummy_span(),
        };
        
        println!("DEBUG: Created duplicate 'foo' items (function and struct)");
        return vec![func_item, struct_item];
    }
    
    // Default: empty list
    println!("DEBUG: No matching pattern found, returning empty item list");
    Vec::new()
}

// --- Helper Function ---
// Parses source and runs resolution, returning the final structure.
fn run_resolution<'db>(db: &'db TestDb, source: &str) -> ResolvedModuleStructure<'db> {
    println!("\nDEBUG: Starting resolution for source: {}", source);
    
    // Parse the source into AST items
    let ast_items = parse_source(source);
    println!("DEBUG: Parsed {} AST items", ast_items.len());
    
    // Create module unit using our tracked function
    let module_unit = test_create_module_unit(db, "test".to_string(), source.to_string(), ast_items);
    println!("DEBUG: Created module unit");
    
    // Run resolver - let it handle definitions and scopes naturally
    println!("DEBUG: Running resolver...");
    let result = db.resolve_definitions(module_unit);
    println!("DEBUG: Resolver completed");
    
    result
}

// Helper to print errors nicely
fn report_diagnostics(db: &TestDb, resolved: &ResolvedModuleStructure) {
     let errors = resolved.errors(db);
     let warnings = resolved.warnings(db);

     if !errors.is_empty() || !warnings.is_empty() {
         println!("\n--- Diagnostics for Test ---");
     }

     if !errors.is_empty() {
         println!("Resolution Errors ({}):", errors.len());
         for (i, error) in errors.iter().enumerate() {
             // Simple debug print
             println!("- Error {}: {:?}", i, error);
         }
     }
     
     if !warnings.is_empty() {
         println!("Resolution Warnings ({}):", warnings.len());
         for (i, warning) in warnings.iter().enumerate() {
             // Simple debug print
             println!("- Warning {}: {:?}", i, warning);
         }
     }
     
     if !errors.is_empty() || !warnings.is_empty() {
         println!("--------------------------");
     }
}

// --- Integration Tests ---

#[test]
fn test_empty_input() {
    println!("\n=== TEST: test_empty_input ===");
    let db = TestDb::default();
    let resolved = run_resolution(&db, "");
    report_diagnostics(&db, &resolved);
    
    let defs = resolved.definitions(&db);
    println!("DEBUG: Checking empty input results:");
    println!("  - errors: {}", resolved.errors(&db).len());
    println!("  - warnings: {}", resolved.warnings(&db).len());
    println!("  - structs: {}", defs.structs.len());
    println!("  - enums: {}", defs.enums.len());
    println!("  - functions: {}", defs.functions.len());
    println!("  - traits: {}", defs.traits.len());
    println!("  - impls: {}", defs.impls.len());
    
    assert!(resolved.errors(&db).is_empty());
    assert!(resolved.warnings(&db).is_empty());
    assert!(defs.structs.is_empty());
    assert!(defs.enums.is_empty());
    assert!(defs.functions.is_empty(), "Expected zero functions after filtering");
    assert!(defs.traits.is_empty(), "Expected zero traits after filtering");
    assert!(defs.impls.is_empty());
}

#[test]
fn test_simple_struct() {
    println!("\n=== TEST: test_simple_struct ===");
    let db = TestDb::default();
    let source = "struct Point { x: i32, y: i32 }";
    let resolved = run_resolution(&db, source);
    report_diagnostics(&db, &resolved);
    
    println!("DEBUG: Checking struct test results:");
    println!("  - errors: {}", resolved.errors(&db).len());
    println!("  - warnings: {}", resolved.warnings(&db).len());
    
    let defs = resolved.definitions(&db);
    println!("  - structs: {}", defs.structs.len());
    
    if !defs.structs.is_empty() {
        println!("  - struct name: {}", defs.structs[0].name);
        println!("  - fields count: {}", defs.structs[0].fields.len());
        if defs.structs[0].fields.len() >= 1 {
            println!("  - field[0] name: {}", defs.structs[0].fields[0].name);
            println!("  - field[0] type: {:?}", defs.structs[0].fields[0].field_type);
        }
        if defs.structs[0].fields.len() >= 2 {
            println!("  - field[1] name: {}", defs.structs[0].fields[1].name);
            println!("  - field[1] type: {:?}", defs.structs[0].fields[1].field_type);
        }
    }
    
    assert!(resolved.errors(&db).is_empty(), "Expected no errors");
    assert!(resolved.warnings(&db).is_empty(), "Expected no warnings");

    let defs = resolved.definitions(&db);
    assert_eq!(defs.structs.len(), 1, "Expected one struct");
    let point_struct = &defs.structs[0];
    assert_eq!(point_struct.name, "Point");
    assert_eq!(point_struct.fields.len(), 2);
    assert_eq!(point_struct.fields[0].name, "x");
    assert_eq!(point_struct.fields[0].field_type, ResolvedType::Primitive(PrimitiveType::I32));
    assert_eq!(point_struct.fields[1].name, "y");
    assert_eq!(point_struct.fields[1].field_type, ResolvedType::Primitive(PrimitiveType::I32));
    assert!(point_struct.generic_params.is_empty());
}

#[test]
fn test_simple_function_signature() {
    println!("\n=== TEST: test_simple_function_signature ===");
    let db = TestDb::default();
    let source = "fn add(a: f32, b: f32) -> f32;"; // Signature only for now
    let resolved = run_resolution(&db, source);
    report_diagnostics(&db, &resolved);
    
    println!("DEBUG: Checking function test results:");
    println!("  - errors: {}", resolved.errors(&db).len());
    println!("  - warnings: {}", resolved.warnings(&db).len());
    
    let defs = resolved.definitions(&db);
    println!("  - functions: {}", defs.functions.len());
    
    // Print detailed error information if we have errors
    if !resolved.errors(&db).is_empty() {
        println!("\nDETAILED ERROR ANALYSIS for function test:");
        for (i, error) in resolved.errors(&db).iter().enumerate() {
            println!("  Error {}: {:?}", i, error);
            
            match error {
                ResolutionError::NameNotFound { name, .. } => {
                    println!("    Name not found: {}", name);
                },
                ResolutionError::TypeMismatch { expected, found, .. } => {
                    println!("    Type mismatch: expected '{}', found '{}'", expected, found);
                },
                ResolutionError::PatternMismatch { message, .. } => {
                    println!("    Pattern mismatch: {}", message);
                },
                _ => println!("    Other error type"),
            }
        }
        println!("END ERROR ANALYSIS\n");
    }
    
    if !defs.functions.is_empty() {
        println!("  - function name: {}", defs.functions[0].name);
        println!("  - parameters count: {}", defs.functions[0].parameters.len());
        if defs.functions[0].parameters.len() >= 1 {
            println!("  - param[0] name: {}", defs.functions[0].parameters[0].name);
            println!("  - param[0] type: {:?}", defs.functions[0].parameters[0].param_type);
        }
        if defs.functions[0].parameters.len() >= 2 {
            println!("  - param[1] name: {}", defs.functions[0].parameters[1].name);
            println!("  - param[1] type: {:?}", defs.functions[0].parameters[1].param_type);
        }
        println!("  - return type: {:?}", defs.functions[0].return_type);
    } else {
        println!("  No functions were resolved!");
    }
    
    // This test focuses on signature resolution (Pass 3)
    assert!(resolved.errors(&db).is_empty(), "Expected no errors in signature resolution");

    let defs = resolved.definitions(&db);
    // Expect the user-defined function only after filtering
    assert_eq!(defs.functions.len(), 1, "Expected exactly one function (add) after filtering"); 
    
    // The root module symbol should be the one for the function itself
    let add_func = &defs.functions[0];
    // let root_module_symbol = add_func.module_symbol;

    // Assertions specific to the 'add' function
    println!("DEBUG: Asserting against found function: {:?}", add_func);
    assert_eq!(add_func.name, "add");
    assert_eq!(add_func.parameters.len(), 2, "Function 'add' should have 2 parameters");
    assert_eq!(add_func.parameters[0].name, "a", "First parameter name mismatch");
    assert_eq!(add_func.parameters[0].param_type, ResolvedType::Primitive(PrimitiveType::F32), "First parameter type mismatch");
    assert_eq!(add_func.parameters[1].name, "b", "Second parameter name mismatch");
    assert_eq!(add_func.parameters[1].param_type, ResolvedType::Primitive(PrimitiveType::F32), "Second parameter type mismatch");
    assert_eq!(add_func.return_type, ResolvedType::Primitive(PrimitiveType::F32), "Return type mismatch");
    assert!(add_func.body.is_none()); // No body provided or resolved yet
    assert!(add_func.generic_params.is_empty());
}

#[test]
fn test_duplicate_definition_error() {
    println!("\n=== TEST: test_duplicate_definition_error ===");
    let db = TestDb::default();
    let source = r#"
fn foo() -> unit;
struct foo { x: f32 }
    "#;
    let resolved = run_resolution(&db, source);
    report_diagnostics(&db, &resolved);
    
    println!("DEBUG: Checking duplicate def test results:");
    println!("  - errors: {}", resolved.errors(&db).len());
    println!("  - warnings: {}", resolved.warnings(&db).len());
    
    let defs = resolved.definitions(&db);
    println!("  - structs: {}", defs.structs.len());
    println!("  - functions: {}", defs.functions.len());
    
    // Print more detailed error info
    for (i, error) in resolved.errors(&db).iter().enumerate() {
        match error {
            ResolutionError::DuplicateDefinition { name, .. } => {
                println!("  - Error {}: DuplicateDefinition for name '{}'", i, name);
            },
            _ => println!("  - Error {}: Non-duplicate error: {:?}", i, error),
        }
    }
    
    assert_eq!(resolved.errors(&db).len(), 1, "Expected one duplicate definition error");
    assert!(matches!(resolved.errors(&db)[0], ResolutionError::DuplicateDefinition { ref name, .. } if name == "foo"));
    assert!(resolved.warnings(&db).is_empty());
    // Depending on error recovery, check what definitions were stored
    // assert_eq!(resolved.definitions(&db).functions.len(), 1); // e.g., first one wins
    // assert_eq!(resolved.definitions(&db).structs.len(), 0);
}

// TODO: Add more integration tests covering all features...