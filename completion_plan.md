# Parallax Completion Plan: Path to MVP

## Completed Tasks

- [x] Implement tokenizer
- [x] Implement parser for basic expressions
- [x] Implement parser for statements
- [x] Implement parser for functions/module structure
- [x] Create initial type system
- [x] Add module and symbol management system
- [x] Implement type resolution for expressions
- [x] Implement module resolution and basic scoping
- [x] Fix module scope handling to correctly store modules in parent scope
- [x] Fix function registration and resolution within modules
- [x] Implement basic import resolution for modules
- [x] Fix glob imports to correctly import symbols from modules
- [x] Add comprehensive testing for module and function resolution

## MVP Requirements

### Core Functionality (Must-Have)

- [ ] Fix core resolver issues:
  - [x] Proper function registration in module scopes
  - [x] Correct module and symbol lookup
  - [ ] Path resolution for nested modules
  - [ ] Complete import resolution from external modules
- [ ] Basic Type System:
  - [x] Simple type resolution
  - [ ] Function type checking
  - [ ] Type inference for basic expressions
- [ ] IR Generation:
  - [ ] Convert AST to simple IR
  - [ ] Handle basic control flow
- [ ] Code Generation:
  - [ ] Generate LLVM IR for simple functions
  - [ ] Compile and run basic programs

### Nice-to-Have (Post-MVP)

- [ ] Generic parameter support
- [ ] Method resolution and trait implementation
- [ ] Overload resolution
- [ ] Advanced type features (trait bounds, complex generics)
- [ ] Optimize IR and generate more efficient code
- [ ] Complete error recovery and reporting
- [ ] Implement concurrency features through interaction networks

## Immediate Next Steps

1. Complete core resolver fixes:
   - Test and fix path resolution for nested modules 
   - Ensure all symbols are correctly registered in their scopes
   - Implement cross-module imports

2. Implement basic type checking for functions:
   - Add type checking for function parameters and return types
   - Implement type inference for basic expressions
   - Add tests for type checking

3. Start on simple IR generation:
   - Define a minimal IR format
   - Implement conversion from AST to IR for basic expressions
   - Add support for function definition in IR

4. Set up basic LLVM integration:
   - Define LLVM bindings
   - Generate LLVM IR for simple functions
   - Set up compilation pipeline

This focused approach will get us to a working MVP that can compile and run simple Parallax programs before expanding to more advanced language features. 