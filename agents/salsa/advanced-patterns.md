# Advanced Patterns

This document covers advanced usage patterns and techniques for Salsa 0.23, based on analysis of complex examples and real-world usage patterns.

## Accumulator Pattern

Accumulators allow you to collect values during query execution, useful for gathering diagnostics, warnings, or other side information.

### Basic Accumulator Usage
```rust
#[salsa::accumulator]
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub level: DiagnosticLevel,
    pub message: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum DiagnosticLevel {
    Error,
    Warning,
    Info,
}

#[salsa::tracked]
fn check_function(db: &dyn Database, func: Function) {
    // Validate function signature
    if func.name(db).is_empty() {
        Diagnostic {
            level: DiagnosticLevel::Error,
            message: "Function name cannot be empty".to_string(),
            span: func.name_span(db),
        }.accumulate(db);
    }
    
    // Check function body
    check_statements(db, func.body(db));
}

#[salsa::tracked]
fn check_statements(db: &dyn Database, statements: Vec<Statement>) {
    for stmt in statements {
        match stmt.kind() {
            StatementKind::Assignment { target, value } => {
                if is_unused_variable(db, target) {
                    Diagnostic {
                        level: DiagnosticLevel::Warning,
                        message: format!("Unused variable: {}", target.name()),
                        span: target.span(),
                    }.accumulate(db);
                }
            }
            _ => {}
        }
    }
}

// Collect all diagnostics
fn analyze_program(db: &dyn Database, program: Program) -> Vec<Diagnostic> {
    for func in program.functions(db) {
        check_function(db, func);
    }
    
    // Collect all accumulated diagnostics
    check_function::accumulated::<Diagnostic>(db, program.main_function(db))
}
```

### Accumulator Chaining
```rust
#[salsa::accumulator]
#[derive(Debug, Clone)]
pub struct UsedSymbol {
    pub symbol: Symbol,
    pub location: Span,
}

#[salsa::tracked]
fn analyze_expression(db: &dyn Database, expr: Expression) {
    match expr.kind(db) {
        ExpressionKind::Variable(symbol) => {
            // Accumulate symbol usage
            UsedSymbol {
                symbol,
                location: expr.span(db),
            }.accumulate(db);
        }
        ExpressionKind::Call { function, args } => {
            // Analyze function call
            analyze_expression(db, function);
            for arg in args {
                analyze_expression(db, arg);  // Accumulators chain automatically
            }
        }
        _ => {}
    }
}

#[salsa::tracked]
fn find_unused_symbols(db: &dyn Database, program: Program) -> Vec<Symbol> {
    // Analyze all expressions to collect used symbols
    for func in program.functions(db) {
        analyze_expression(db, func.body(db));
    }
    
    let used_symbols: HashSet<Symbol> = analyze_expression::accumulated::<UsedSymbol>(db, program.main_function(db))
        .into_iter()
        .map(|usage| usage.symbol)
        .collect();
    
    // Find declared but unused symbols
    program.declared_symbols(db)
        .into_iter()
        .filter(|symbol| !used_symbols.contains(symbol))
        .collect()
}
```

## Cycle Recovery Strategies

Salsa provides sophisticated cycle detection and recovery mechanisms for handling recursive dependencies.

### Basic Cycle Recovery
```rust
#[salsa::tracked(cycle = recover)]
fn compute_size(db: &dyn Database, node: Node) -> usize {
    let mut size = 1;
    for child in node.children(db) {
        size += compute_size(db, child);  // May create cycle
    }
    size
}

fn recover_compute_size(
    db: &dyn Database,
    cycle: &salsa::Cycle,
    node: Node,
) -> usize {
    eprintln!("Cycle detected computing size for {:?}", node);
    
    // Return a reasonable default for cycles
    1
}
```

### Iterative Cycle Resolution
```rust
#[salsa::tracked(cycle = iterate)]
fn compute_fixed_point(db: &dyn Database, node: Node) -> FixedPointValue {
    let mut value = FixedPointValue::initial();
    
    for dependency in node.dependencies(db) {
        let dep_value = compute_fixed_point(db, dependency);
        value = value.combine(dep_value);
    }
    
    value.normalize()
}

fn iterate_compute_fixed_point(
    db: &dyn Database,
    cycle: &salsa::Cycle,
    node: Node,
    current_value: &FixedPointValue,
    iteration: u32,
) -> salsa::CycleRecoveryAction<FixedPointValue> {
    if iteration > 100 {
        // Prevent infinite iteration
        salsa::CycleRecoveryAction::Fallback(FixedPointValue::default())
    } else if current_value.has_converged() {
        // Fixed point reached
        salsa::CycleRecoveryAction::Fallback(current_value.clone())
    } else {
        // Continue iterating
        salsa::CycleRecoveryAction::Iterate
    }
}
```

### Cycle-Aware Algorithms
```rust
#[salsa::tracked(cycle = recover)]
fn strongly_connected_components(db: &dyn Database, graph: Graph) -> Vec<Component> {
    let mut components = Vec::new();
    let mut visited = HashSet::new();
    
    for node in graph.nodes(db) {
        if !visited.contains(&node) {
            let component = find_component(db, node, &mut visited);
            components.push(component);
        }
    }
    
    components
}

fn recover_strongly_connected_components(
    db: &dyn Database,
    cycle: &salsa::Cycle,
    graph: Graph,
) -> Vec<Component> {
    // When cycle detected, create component from cycle participants
    let cycle_nodes: Vec<Node> = cycle.participant_keys()
        .iter()
        .filter_map(|key| extract_node_from_key(key))
        .collect();
    
    vec![Component::new(cycle_nodes)]
}
```

## Parallel Processing Patterns

### Parallel Map Operations
```rust
#[salsa::tracked]
fn process_files_parallel(db: &dyn Database, files: Vec<SourceFile>) -> Vec<ProcessedFile> {
    // Process files in parallel using Salsa's par_map
    salsa::par_map(files, |file| {
        process_single_file(db, file)
    })
}

#[salsa::tracked]
fn process_single_file(db: &dyn Database, file: SourceFile) -> ProcessedFile {
    let ast = parse_file(db, file);
    let analyzed = analyze_ast(db, ast);
    let optimized = optimize_ast(db, analyzed);
    
    ProcessedFile::new(db, file, optimized)
}
```

### Fork-Join Patterns
```rust
#[salsa::tracked]
fn parallel_analysis(db: &dyn Database, program: Program) -> AnalysisResult {
    let (syntax_result, semantic_result) = salsa::join(
        || syntax_analysis(db, program),
        || semantic_analysis(db, program),
    );
    
    combine_results(syntax_result, semantic_result)
}
```

### Work Distribution
```rust
#[salsa::tracked]
fn distributed_compilation(db: &dyn Database, project: Project) -> CompilationResult {
    let modules = project.modules(db);
    
    // Partition modules by dependency levels
    let levels = compute_dependency_levels(db, modules);
    
    let mut results = Vec::new();
    
    // Process each level in parallel
    for level in levels {
        let level_results = salsa::par_map(level, |module| {
            compile_module(db, module)
        });
        results.extend(level_results);
    }
    
    link_modules(db, results)
}
```

## Advanced Database Patterns

### Multi-Database Coordination
```rust
pub struct CompilerSystem {
    syntax_db: SyntaxDatabase,
    semantic_db: SemanticDatabase,
    codegen_db: CodegenDatabase,
}

impl CompilerSystem {
    pub fn compile(&mut self, source: SourceFile) -> Binary {
        // Phase 1: Syntax analysis
        let ast = parse_file(&self.syntax_db, source);
        
        // Phase 2: Semantic analysis (depends on syntax)
        let typed_ast = type_check(&self.semantic_db, ast);
        
        // Phase 3: Code generation (depends on semantics)
        let binary = generate_code(&self.codegen_db, typed_ast);
        
        binary
    }
    
    pub fn invalidate_syntax(&mut self, file: SourceFile) {
        // Invalidate only syntax database
        file.set_text(&mut self.syntax_db).to(new_content);
        
        // Other databases will be invalidated transitively
    }
}
```

### Database Snapshots for Isolation
```rust
pub struct LanguageServer {
    db: Arc<Mutex<LanguageDatabase>>,
}

impl LanguageServer {
    pub fn handle_completion_request(&self, position: Position) -> Vec<Completion> {
        // Create snapshot for read-only access
        let db_snapshot = {
            let db = self.db.lock().unwrap();
            db.snapshot()
        };
        
        // Use snapshot in background thread
        std::thread::spawn(move || {
            compute_completions(&*db_snapshot, position)
        }).join().unwrap()
    }
    
    pub fn handle_file_change(&self, file: SourceFile, new_content: String) {
        // Modify main database
        let mut db = self.db.lock().unwrap();
        file.set_text(&mut *db).to(new_content);
    }
}
```

## Complex Query Patterns

### Conditional Dependencies
```rust
#[salsa::tracked]
fn compile_with_features(db: &dyn Database, module: Module, config: Config) -> CompiledModule {
    let ast = parse_module(db, module);
    
    let processed_ast = if config.feature_enabled(db, "macros") {
        // Only depends on macro expansion if feature is enabled
        expand_macros(db, ast)
    } else {
        ast
    };
    
    let optimized_ast = if config.optimization_level(db) > 0 {
        // Only depends on optimizer if optimization is enabled
        optimize_ast(db, processed_ast, config.optimization_level(db))
    } else {
        processed_ast
    };
    
    generate_code(db, optimized_ast)
}
```

### Dynamic Query Selection
```rust
#[salsa::tracked]
fn analyze_with_strategy(db: &dyn Database, code: Code, strategy: AnalysisStrategy) -> Analysis {
    match strategy {
        AnalysisStrategy::Fast => fast_analysis(db, code),
        AnalysisStrategy::Thorough => thorough_analysis(db, code),
        AnalysisStrategy::Custom(params) => custom_analysis(db, code, params),
    }
}

#[salsa::tracked]
fn adaptive_analysis(db: &dyn Database, code: Code) -> Analysis {
    // Choose strategy based on code characteristics
    let strategy = if code.size(db) > 10000 {
        AnalysisStrategy::Fast
    } else if code.complexity(db) > 50 {
        AnalysisStrategy::Thorough
    } else {
        AnalysisStrategy::Custom(default_params())
    };
    
    analyze_with_strategy(db, code, strategy)
}
```

### Query Result Transformation
```rust
#[salsa::tracked]
fn transform_results<T, U>(
    db: &dyn Database,
    inputs: Vec<T>,
    transformer: fn(&dyn Database, T) -> U,
) -> Vec<U>
where
    T: Send + Sync + Clone,
    U: Send + Sync,
{
    salsa::par_map(inputs, |input| transformer(db, input))
}

// Usage
#[salsa::tracked]
fn process_all_files(db: &dyn Database, files: Vec<SourceFile>) -> Vec<ProcessedFile> {
    transform_results(db, files, |db, file| {
        let ast = parse_file(db, file);
        let checked = type_check(db, ast);
        ProcessedFile::new(db, file, checked)
    })
}
```

## Memory Management Patterns

### Custom LRU Strategies
```rust
pub struct SmartDatabase {
    storage: salsa::Storage<Self>,
    memory_pressure: Arc<AtomicBool>,
}

impl SmartDatabase {
    pub fn monitor_memory(&self) {
        std::thread::spawn({
            let db = self.clone();
            let pressure = self.memory_pressure.clone();
            
            move || {
                loop {
                    std::thread::sleep(Duration::from_secs(10));
                    
                    if get_memory_usage() > MEMORY_THRESHOLD {
                        pressure.store(true, Ordering::Relaxed);
                        db.trigger_lru_eviction();
                    } else {
                        pressure.store(false, Ordering::Relaxed);
                    }
                }
            }
        });
    }
    
    pub fn aggressive_cleanup(&mut self) {
        // Force cleanup of all non-essential caches
        self.synthetic_write(salsa::Durability::MIN);
        self.trigger_lru_eviction();
    }
}
```

### Hierarchical Caching
```rust
#[salsa::tracked]
fn expensive_computation(db: &dyn Database, input: LargeInput) -> ExpensiveResult {
    // Break down into smaller, cacheable pieces
    let parts = input.partition(db);
    
    let partial_results = salsa::par_map(parts, |part| {
        compute_part(db, part)  // Each part cached separately
    });
    
    combine_partial_results(partial_results)
}

#[salsa::tracked]
fn compute_part(db: &dyn Database, part: InputPart) -> PartialResult {
    // Smaller computation that's more likely to be reused
    expensive_operation(part.data())
}
```

## Error Handling Patterns

### Graceful Degradation
```rust
#[salsa::tracked]
fn robust_analysis(db: &dyn Database, file: SourceFile) -> AnalysisResult {
    match parse_file(db, file) {
        Ok(ast) => {
            match type_check(db, ast) {
                Ok(typed_ast) => AnalysisResult::Success(typed_ast),
                Err(type_errors) => AnalysisResult::PartialSuccess {
                    ast,
                    errors: type_errors,
                },
            }
        }
        Err(parse_errors) => AnalysisResult::ParseFailure(parse_errors),
    }
}
```

### Error Recovery with Accumulators
```rust
#[salsa::accumulator]
#[derive(Debug, Clone)]
pub struct RecoveryAction {
    pub error: CompilationError,
    pub recovery: RecoveryStrategy,
}

#[salsa::tracked]
fn parse_with_recovery(db: &dyn Database, file: SourceFile) -> Ast {
    let mut parser = Parser::new(file.text(db));
    let mut ast_builder = AstBuilder::new();
    
    while !parser.is_at_end() {
        match parser.parse_statement() {
            Ok(stmt) => ast_builder.add_statement(stmt),
            Err(error) => {
                // Record error and recovery action
                RecoveryAction {
                    error: error.clone(),
                    recovery: RecoveryStrategy::SkipToNextStatement,
                }.accumulate(db);
                
                // Attempt recovery
                parser.recover_to_next_statement();
                ast_builder.add_error_node(error);
            }
        }
    }
    
    ast_builder.build()
}
```

## Testing Patterns

### Property-Based Testing
```rust
#[cfg(test)]
mod property_tests {
    use super::*;
    use proptest::prelude::*;
    
    proptest! {
        #[test]
        fn test_parse_roundtrip(source in ".*") {
            let mut db = TestDatabase::default();
            let file = SourceFile::new(&db, source.clone());
            
            if let Ok(ast) = parse_file(&db, file) {
                let regenerated = ast.to_source_code(&db);
                // Parsing should be deterministic
                let reparsed = parse_file(&db, SourceFile::new(&db, regenerated));
                prop_assert_eq!(ast, reparsed.unwrap());
            }
        }
        
        #[test]
        fn test_incremental_consistency(
            initial_source in ".*",
            modified_source in ".*"
        ) {
            let mut db = TestDatabase::default();
            let file = SourceFile::new(&db, initial_source);
            
            // Compute initial result
            let result1 = expensive_computation(&db, file);
            
            // Modify and recompute
            file.set_text(&mut db).to(modified_source);
            let result2 = expensive_computation(&db, file);
            
            // Reset to original and recompute
            file.set_text(&mut db).to(initial_source);
            let result3 = expensive_computation(&db, file);
            
            // Should get same result as initially
            prop_assert_eq!(result1, result3);
        }
    }
}
```

### Benchmark-Driven Development
```rust
#[cfg(test)]
mod benchmarks {
    use super::*;
    use criterion::{black_box, Criterion};
    
    pub fn benchmark_incremental_compilation(c: &mut Criterion) {
        let mut db = CompilerDatabase::default();
        let large_file = create_large_test_file();
        
        c.bench_function("initial_compilation", |b| {
            b.iter(|| {
                let mut fresh_db = CompilerDatabase::default();
                let file = SourceFile::new(&fresh_db, large_file.clone());
                black_box(compile_file(&fresh_db, file));
            });
        });
        
        c.bench_function("incremental_compilation", |b| {
            let file = SourceFile::new(&db, large_file.clone());
            compile_file(&db, file); // Prime the cache
            
            b.iter(|| {
                // Make small change
                file.set_text(&mut db).to(make_small_change(&large_file));
                black_box(compile_file(&db, file));
            });
        });
    }
}
```

These advanced patterns enable sophisticated applications while maintaining the benefits of incremental computation and automatic dependency tracking that make Salsa powerful.