# Identifiers and Keywords

## Identifiers

Identifiers follow typical rules:
- Start with a letter or underscore
- Contain letters, digits, or underscores  
- Case-sensitive

```parallax
// Valid identifiers
let valid_name = 1;
let _private = 2;
let camelCase = 3;
let PascalCase = 4;
let SCREAMING_SNAKE = 5;
let name123 = 6;

// Invalid identifiers (would cause errors)
// let 123name = 7;     // Cannot start with digit
// let my-name = 8;     // Hyphens not allowed
// let my name = 9;     // Spaces not allowed
```

## Reserved Keywords

The following words are reserved and cannot be used as identifiers:

### Control Flow
- `if`, `then`, `else` - Conditional expressions
- `match`, `with` - Pattern matching
- `for`, `in` - Iteration (in patterns)

### Declarations
- `const` - Compile-time constants and functions
- `let` - Runtime bindings
- `mut` - Mutable bindings
- `type` - Type declarations
- `trait` - Trait definitions
- `impl` - Implementations
- `enum` - Enumeration types
- `struct` - Structure types

### Modifiers
- `pub` - Public visibility
- `mod` - Module declaration
- `where` - Constraint clauses

### Import/Export
- `import` - Bring items into scope
- `export` - Re-export items
- `as` - Aliasing

### Literals
- `true`, `false` - Boolean literals

### Future Reserved
These keywords are reserved for potential future use:
- `async`, `await` - Asynchronous programming
- `yield` - Generators
- `macro` - Macro system
- `unsafe` - Unsafe blocks
- `move` - Closure capture semantics