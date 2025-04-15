# <div align="center">Parallax: Zero-Cost Parallelism</div>

<!-- <p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="https://raw.githubusercontent.com/parallax-lang/parallax/main/assets/logo-dark.svg">
    <img alt="Parallax Logo" src="https://raw.githubusercontent.com/parallax-lang/parallax/main/assets/logo.svg" height="128">
  </picture>
</p> -->

<div align="center">
<!-- 
[![Build Status](https://img.shields.io/github/actions/workflow/status/parallax-lang/parallax/ci.yml?branch=main&style=flat-square&label=build)](https://github.com/parallax-lang/parallax/actions)
[![Documentation](https://img.shields.io/badge/📚%20docs-parallax-blue.svg?style=flat-square)](https://docs.parallax-lang.org)
[![Discord](https://img.shields.io/discord/1234567890?style=flat-square&label=💬%20discord&color=5865F2)](https://discord.gg/parallax)
[![License](https://img.shields.io/github/license/parallax-lang/parallax?style=flat-square&label=license&color=success)](LICENSE) -->

</div>

<div align="center">
  <strong>Write sequential code. Get parallel execution.</strong>
  <p>A programming language with C-like performance that automatically extracts parallelism through the power of interaction networks.</p>
</div>

---

## 📚 Table of Contents

- [Why Parallax?](#-why-parallax)
  - [Zero-Cost Parallelism](#-zero-cost-parallelism)
  - [C-like Performance](#-c-like-performance)
  - [Type-Safe Concurrency](#️-type-safe-concurrency)
- [Getting Started](#-getting-started)
  - [Installation](#installation)
  - [Hello World](#hello-world)
  - [Package Management](#package-management)
  - [Language Tour](#language-tour)
- [Core Features](#-core-features)
  - [Automatic Parallelism](#automatic-parallelism)
  - [Side Effects](#side-effects)
  - [Type System](#type-system)
- [Grammar](#-grammar)
- [Community](#-community)

## ✨ Why Parallax?

Parallax is a new systems programming language that makes parallel programming automatic without sacrificing single-threaded performance. It combines:

### 🚀 Zero-Cost Parallelism

```rust
// This normal-looking code automatically runs in parallel
fn process_data(items: List<Item>, headers: Map<String, String>) -> Stats = {
    // These operations run concurrently
    let processed = items.map(process);
    let metadata = extract_metadata(headers);
    
    // This runs when both are done
    combine(processed, metadata)
};
```

### 💨 C-like Performance

```rust
// High-level code
fn analyze(data: Array<f64>) -> Stats = {
    let filtered = data
        .filter(|x| x > 0.0)
        .map(|x| x * 2.0);
    
    Stats::new(filtered)
};

// Compiles to efficient machine code via LLVM
```

### 🛡️ Type-Safe Concurrency

```rust
// The compiler ensures:
// - No data races
// - No deadlocks
// - Proper resource cleanup

fn update_database() -> Result<(), Error> = {
    // Parallel reads are safe
    let users = db.query("SELECT * FROM users");
    let posts = db.query("SELECT * FROM posts");
    
    // Sequential writes are enforced by `->`
    db.execute("UPDATE stats SET count = ?", [users.len()]) ->
    db.execute("INSERT INTO logs VALUES (?)", ["Updated"])
};
```

## 🚀 Getting Started

### Hello World

```rust
// src/main.plx
fn main() -> Result<(), Error> = {
    println("Hello, parallel world!")?
};
```

```bash
# Run your program
plx run src/main.plx
```

### Language Tour

Here's a quick tour of Parallax's syntax:

```rust
// Variables and basic types
let x = 42;                     // Type inference
let y: i32 = 42;               // Explicit type
let pi: f64 = 3.14159;         // Floating point
let text = "Hello, 世界!";      // UTF-8 strings
let flag = true;               // Booleans

// Arrays and collections
let nums = [1, 2, 3, 4];       // Array
let set = {"a", "b", "c"};    // Set
let map = {                    // Map
    "name": "Alice",
    "age": 30
};

// Control flow
let status = if connected then "online" else "offline";

if log then {
    println("Logging Enabled");
}

match status {                  // Pattern matching
    "online" => connect()?,
    "offline" => retry()?,
    _ => fail()?
};

// Functions
fn greet(name: String) -> String = format("Hello, {}!", name);
fn add<A, B>(a: A, b: B) -> A + B = a + b;

// Functions with positional and named arguments
fn configure(
    name: String,                    // Required positional
    description: Option<String> = None,     // Optional positional as it has a default value,
    ...rest: List<String>, // The remaining positional args
) -> Config = {
    Config::new( name, description );
    rest.map(|x| x.to_string()).collect()
};

// Closures
let double = |x| => x * 2;                    // No type annotation needed
let typed = |x: i32| -> i32 => { x * 2 };     // Optional type annotations
let multi = |x, y| => x + y;                  // Multiple parameters
let generic = <T>|x: T| => x;                 // Generic parameters

// Structs
struct Point {
    x: f64, // Private by default
    pub y: f64 // Public
}

impl Point {
    // Constructor
    pub fn new(x: f64, y: f64) -> Self = {
        Point { x, y }
    };
    
    // Method
    pub fn distance(self, other: Point) -> f64 = {
        ((self.x - other.x).pow(2) + 
         (self.y - other.y).pow(2)).sqrt()
    };
}

// Enums
enum Option<T> {
    Some(T),
    None
}

// Not what the built-in Result type looks like
enum CustomResult<T, E> {
    Ok(T),
    Err {
        error: E,
        message: String,
    }
}

// Pattern matching on enums
fn process(opt: Option<i32>) -> i32 = match opt {
    Some(x) => x * 2,
    None => 0
};

// Error handling
fn divide(x: f64, y: f64) -> Result<f64, Error> = {
    if y == 0.0 {
        Err(Error::DivideByZero)
    } else {
        Ok(x / y)
    }
};

// Using the ? operator for error propagation
fn compute() -> Result<f64, Error> = {
    let x = read_number()?;     // Returns early on error
    let y = read_number()?;
    Ok(divide(x, y))
};

// Modules
mod math {
    pub fn add(x: i32, y: i32) = x + y;
    fn helper() -> String = "private";    // Private by default
}

// Rust-like Traits
trait Display {
    fn display(self) -> String;
}

impl Display for Point {
    fn display(self) -> String = format("({}, {})", self.x, self.y);
}

// Generics
struct Pair<T> {
    first: T,
    second: T
}

impl<T> Pair<T> {
    fn swap(self) -> Self = {
        Pair {
            first: self.second,
            second: self.first
        }
    }
}

// Trait bounds
fn print<T: Display>(value: T) -> Result<(), Error> = {
    println(value.display())?
};
```

## 📚 Core Features

### Automatic Parallelism

Parallax automatically extracts parallelism through interaction networks - no manual thread management required:

```rust
// This code:
fn process_batch(items: List<Item>) -> Stats = {
    let processed = items.map(transform);
    let filtered = processed.filter(validate);
    aggregate(filtered)
};

// Becomes this interaction network:
//
//    items ──┬──> transform ──> filter ──┐
//            │                           ├──> aggregate
//            └─> validate ───────────────┘
```

The runtime automatically:
- Distributes work across cores
- Manages data dependencies
- Optimizes memory usage
- Handles backpressure

### Side Effects

Parallax tracks side effects at compile time to ensure safe parallelization:

```rust
// Effect tracking ensures proper ordering
fn process_file(path: Path) -> Result<(), Error> = {
    // Read effect
    let content = read_file(path)?;
    
    // Pure computation - can run anytime
    let processed = transform(content);
    
    // Write effects must be ordered. As they look independent to the compiler, you can use -> to enforce ordering.
    write_file("log.txt", "Starting")? ->
    write_file(path, processed)? ->
    write_file("log.txt", "Complete")?
};

// Resource cleanup is automatic
fn with_temp_file(operation: fn(Path) -> Result<(), Error>) -> Result<(), Error> = {
    let temp = create_temp_file()?;
    
    // temp is automatically deleted after operation completes or fails
    operation(temp.path)?
};
```

### Type System

Parallax combines modern type system features with zero-cost abstractions:

#### Affine Types

```rust
// Resources can't be used more than once
fn process_connection(conn: Connection) -> Result<(), Error> = {
    send_data(conn)?;    // conn is moved here
    send_more(conn)?;    // Error: conn was moved
};

// Use clone for explicit duplication
fn broadcast(conn: Connection) -> Result<(), Error> = {
    let (conn1, conn2) = dup(conn);
    send_to_primary(conn)?;
    send_to_backup(conn2)?
};

// Copy types are automatically duplicated
fn example<T: Copy>(x: T) -> (T, T) where T: Copy = (x, x);
```

#### Built-in Types

```rust
// Arrays - fixed-size contiguous memory
let nums: Array<i32, 4> = [1, 2, 3, 4];

// Strings - UTF-8 encoded
let greeting: String = "Hello, 世界!";

// Maps - hash-based key-value store
let config: Map<String, Value> = {
    "host": "localhost",
    "port": 8080
};
```

#### Algebraic Data Types

```rust
// Sum types (enums)
enum Result<T, E> {
    Ok(T),
    Err(E)
}

// Product types (structs)
struct User {
    id: UserId,
    name: String,
    roles: Set<Role>
}

// Generalized Algebraic Data Types
enum Expr<T> {
    Int(i32) -> Expr<i32>,
    Add(Expr<i32>, Expr<i32>) -> Expr<i32>,
    If(Expr<bool>, Expr<T>, Expr<T>) -> Expr<T>
}
```


#### Type Inference

```rust
// Types are inferred automatically
let nums = [1, 2, 3];           // Array<i32, 3>
let doubled = nums.map(|x| x * 2); // Array<i32, 3>

// Complex types are inferred
let result = items
    .filter(|x| x.valid())
    .map(process)
    .collect();  // Type inferred from context

// Generic functions
fn first<T>(list: List<T>) -> Option<T> = match list {
    Nil => None,
    Cons(x, xs) => Some(x)
};

```


## 📖 Grammar

The complete grammar of Parallax in EBNF notation:

```ebnf
# Top-level Constructs
SourceFile     ::= Item*
Item        ::= Visibility? (Function | TypeDef | Enum | Struct | Trait | Impl | Module | Use)
BlockItem   ::= Function | TypeDef | Enum | Struct | Use | Expr ";"
Module      ::= "mod" Identifier "{" Item* "}"

# Use statements
Use         ::= "use" UseItem ("::" UseItem)* ";"  # e.g., use std::io::Result;
UseItem     ::= PathSegment ("as" Identifier)?     # e.g., use foo as bar
              | "{" UseItem ("," UseItem)* "}"     # e.g., use std::{io, fs}
              | "*"                                # e.g., use std::prelude::*

# Basic Elements
Identifier  ::= [a-zA-Z_][a-zA-Z0-9_]*
Visibility  ::= "pub"?

# Functions
FunctionSig ::= "fn" Identifier GenericParameters? 
                "(" Parameters? ")" 
                ("->" Type)? 
                WhereClause?              # e.g. fn foo<T>(x: T) -> u32 where T: Display
Function    ::= FunctionSig "=" Expr ";"

# Type Definitions
TypeDef     ::= "type" Identifier GenericParameters? "=" Type WhereClause? ";"  # e.g. type Meters<T> = T where T: Number;
Enum        ::= "enum" Identifier GenericParameters? WhereClause? "{" EnumVariants? "}"
Struct      ::= "struct" Identifier GenericParameters? WhereClause? StructBody
Trait       ::= "trait" Identifier GenericParameters? WhereClause? 
                (":" TraitBounds)? 
                "{" TraitItems? "}"       # e.g. trait Show: Display { fn show(&self) -> String; }
Impl        ::= "impl" GenericParameters? Type 
                ("for" Type)? 
                WhereClause? 
                "{" ImplItems? "}"        # e.g. impl<T> Show for Vec<T> where T: Display { ... }

# Generics and Constraints
GenericParameters ::= "<" (GenericParam ("," GenericParam)* ","?)? ">"
GenericParam  ::= "phantom"? Identifier (":" Kind)?  # e.g. phantom T: * -> *
WhereClause  ::= "where" WherePred ("," WherePred)* ","?
WherePred    ::= Type ":" Path ("+" Path)*          # e.g., T: Display + Clone

# Kinds and Types
Kind         ::= "*" | FunctionKind | TupleKind     # e.g., * -> * for functors
FunctionKind ::= Kind "->" Kind
TupleKind    ::= "(" Kind ("," Kind)* ","? ")"

Type         ::= Path | FunctionType | TupleType | KindApp | ArrayType | "Self"
FunctionType ::= Type "->" Type
TupleType    ::= "(" (Type ("," Type)* ","?)? ")"      # e.g., (i32, String)
KindApp      ::= Type "<" (Type ("," Type)* ","?)? ">"  # e.g., Option<T>
ArrayType    ::= "[" Type ";" Expression "]"         # e.g., [u8; 4]

# Data Structure Components
EnumVariants ::= EnumVariant ("," EnumVariant)* ","?
EnumVariant  ::= Identifier (StructBody | TupleBody)?  # e.g., Some(T) or Socket { addr: String, port: u16 } or None

# Common Structure Components
StructBody   ::= "{" (StructField ("," StructField)* ","?)? "}"  # Used in type definitions
StructField  ::= Identifier ":" Type
TupleBody    ::= "(" (Type ("," Type)* ","?)? ")"               # Used in type definitions

# Struct and Enum Creation
StructExpr    ::= Path StructBody  # Tuple body is just a function call

# Trait and Implementation Items
TraitItems   ::= TraitItem ("," TraitItem)* ","?
ImplItems    ::= ImplItem ("," ImplItem)* ","?
TraitItem    ::= FunctionSig ("=" Expr)?
ImplItem     ::= Function

# Parameters
Parameters   ::= (Parameter ("," Parameter)* ","?)?
Parameter    ::= "self" | "..."? Identifier (":" Type)? ("=" Expr)?

# Expression Structure Components
ExprStructBody ::= "{" (FieldInit ("," FieldInit)* ","? BaseStruct?)? "}"  # Used in expressions
FieldInit     ::= Identifier ":" Expression | Identifier  # Shorthand if names match
BaseStruct    ::= ".." Expression
ExprTupleBody ::= "(" (Expression ("," Expression)* ","?)? ")"   # Used in expressions

# Collections
ArrayExpr    ::= "[" (Expression ("," Expression)* ","?)? "]"
TupleExpr    ::= "(" (                               # e.g., (), (1,), (1, 2)
                   Expression "," Expression ("," Expression)* ","?
                 | Expression ","
                 | empty
                 ) ")"
MapExpr      ::= "#{" (MapEntry ("," MapEntry)* ","?)? "}"  # e.g., #{ "x": 1, "y": 2 }
HashSetExpr  ::= "#[" (Expression ("," Expression)* ","?)? "]"  # e.g., #[ "x", "y" ]
MapEntry     ::= Expression ":" Expression

# Expressions
Expression  ::= Block 
               | IfExpr 
               | MatchExpr 
               | BinaryExpr 
               | UnaryExpr 
               | CallExpr 
               | LambdaExpr 
               | Literal 
               | Path
               | FieldAccess 
               | ArrayExpr 
               | TupleExpr
               | MapExpr 
               | HashSetExpr
               | LetExpr
               | StructExpr
               | "(" Expression ")"
Block        ::= "{" (BlockItem)* Expression? "}"
IfExpr       ::= "if" Expression "then" Expression ("else" Expression)?  # e.g., if x > 0 then 1 else -1
MatchExpr    ::= "match" Expression "{" MatchArms? "}"
MatchArms    ::= MatchArm ("," MatchArm)* ","?
MatchArm     ::= Pattern "=>" Expression
LambdaExpr   ::= GenericParameters? "|" Parameters? "|" "=>" Expression
BinaryExpr   ::= Expression BinaryOp Expression
UnaryExpr    ::= UnaryOp Expression
LetExpr      ::= "let" Pattern (":" Type)? "=" Expression
FieldAccess ::= Expression "." (Identifier | DecimalLiteral)

# Literals and Basic Types
Literal      ::= IntegerLiteral
               | FloatLiteral
               | StringLiteral
               | CharacterLiteral
               | BooleanLiteral


IntegerLiteral ::= DecimalLiteral IntegerSuffix?      # e.g., 42u8
                | HexLiteral IntegerSuffix?           # e.g., 0xFF
                | OctalLiteral IntegerSuffix?         # e.g., 0o777
                | BinaryLiteral IntegerSuffix?        # e.g., 0b1010

DecimalLiteral ::= [0-9] ([0-9_]* [0-9])?            # e.g., 1_000_000
HexLiteral     ::= "0x" [0-9a-fA-F] ([0-9a-fA-F_]* [0-9a-fA-F])?
OctalLiteral   ::= "0o" [0-7] ([0-7_]* [0-7])?
BinaryLiteral  ::= "0b" [0-1] ([0-1_]* [0-1])?

IntegerSuffix  ::= "i8" | "i16" | "i32" | "i64" | "i128" | "isize"
                 | "u8" | "u16" | "u32" | "u64" | "u128" | "usize"

FloatLiteral   ::= DecimalFloat FloatSuffix?          # e.g., 3.14f32
                | ExponentFloat FloatSuffix?          # e.g., 1e-10f64

DecimalFloat   ::= [0-9] ([0-9_]* [0-9])? "." [0-9] ([0-9_]* [0-9])?
ExponentFloat  ::= (DecimalFloat | DecimalLiteral) [eE] [+-]? DecimalLiteral

FloatSuffix    ::= "f32" | "f64"

# String Literals
StringLiteral  ::= RawStringLiteral                   # e.g., r#"Hello "world"#
                | NormalStringLiteral                 # e.g., "Hello\nWorld"
                | ByteStringLiteral                   # e.g., b"bytes"

RawStringLiteral ::= "r" "#"* "\"" RawStringContent "\"" "#"*
RawStringContent ::= [^"]*  # Any characters except quote

NormalStringLiteral ::= "\"" StringContent* "\""
ByteStringLiteral   ::= "b\"" StringContent* "\""
StringContent      ::= RegularChar | EscapeSequence
RegularChar        ::= [^"\\]  # Any char except quote or backslash
EscapeSequence     ::= "\\" (
                      ['"\\nrt0] |           # Simple escapes
                      "x" [0-9a-fA-F]{2} |   # Hex escape
                      "u{" [0-9a-fA-F]{1,6} "}" |  # Unicode escape
                      "\n" [ \t]*            # Line continuation
                    )

CharacterLiteral ::= "'" Character "'"
Character        ::= "\\" . | [^'\\]
BooleanLiteral   ::= "true" | "false"

# Operators with Precedence
BinaryOp     ::=  ArrowOp         # Lowest precedence
               | LogicalOrOp
               | LogicalAndOp
               | ComparisonOp
               | AdditionOp
               | MultiplicationOp
               | CastOp          # Highest precedence

ArrowOp       ::= "->"           # For effect sequencing
LogicalOrOp   ::= "||"
LogicalAndOp  ::= "&&"
ComparisonOp  ::= "==" | "!=" | "<" | "<=" | ">" | ">="
AdditionOp    ::= "+" | "-" | "|" | "^"
MultiplicationOp ::= "*" | "/" | "%" | "&" | "<<" | ">>"
CastOp        ::= "as"
UnaryOp      ::= "-" | "!" | "&" | "*"

# Function Calls
CallExpr     ::= Expression "(" CallArgs? ")"
CallArgs     ::= (Argument ("," Argument)* ","?)
Argument     ::= PositionalArg | NamedArg | SpreadArg
PositionalArg ::= Expression
NamedArg     ::= Identifier ":" Expression
SpreadArg    ::= "..." Expression

# Patterns
Pattern      ::= Identifier 
               | Literal 
               | Constructor 
               | ArrayPattern 
               | TuplePattern 
               | RestPattern 
               | OrPattern
               | WildcardPattern
               | StructPattern

Constructor  ::= @Path (TuplePattern | StructPattern)
TuplePattern ::= "(" (Pattern ("," Pattern)* ","?)? ")"
ArrayPattern ::= "[" (Pattern ("," Pattern)* ","?)? "]"
StructPattern::= "{" (FieldPattern ("," FieldPattern)* ","?)? "}"
RestPattern  ::= ".."
WildcardPattern ::= "_"
OrPattern    ::= Pattern ("|" Pattern)*
FieldPattern ::= Identifier (":" Pattern)?  # If no pattern given, uses Identifier as pattern

# Paths
Path         ::= "::"? PathSegment ("::" PathSegment)*  # e.g., std::collections::HashMap
PathSegment  ::= "crate" | "super" | Identifier
GenericArgs  ::= "<" (Type ("," Type)* ","?)? ">"
```

## Development Status

This section outlines the current implementation status of each component in the Parallax ecosystem and what remains to be completed.

### Core Crates Status

| Crate | Status | What's Implemented | What's Left To Do |
|-------|--------|-------------------|-------------------|
| `parallax-lang` | 🟡 Partial | - AST definitions<br>- Visitor pattern<br>- Error handling<br>- Location tracking | - Complete parser implementation<br>- Full language feature support<br>- Comprehensive error recovery |
| `parallax-resolve` | 🟡 Partial | - Symbol table<br>- Namespace management<br>- Import resolution<br>- Error reporting | - Complete resolver implementation<br>- Module system resolution<br>- Path handling |
| `parallax-typeck` | 🟢 Progress | - Type context setup<br>- Basic unification<br>- Type system foundation<br>- Error diagnostics | - Trait system implementation<br>- Full HM type inference<br>- Generic parameter handling<br>- Subtyping and variance |
| `parallax-ir` | 🔴 Early | - IR module structure | - Core IR definition<br>- IR generation from resolved AST<br>- IR optimization passes<br>- SSA form implementation |
| `parallax-mir` | 🔴 Early | - Basic directory structure | - MIR definition<br>- MIR generation from IR<br>- Control flow analysis<br>- Effect tracking system |
| `parallax-codegen` | 🔴 Early | - Project structure | - Interaction net code generation<br>- LLVM binding setup<br>- LLVM IR generation<br>- Target-specific optimizations |
| `parallax-net` | 🟡 Progress | - Core runtime architecture<br>- Node/Port/Partition system<br>- Worker implementation<br>- Network structure | - Complete reduction rule implementation<br>- Full garbage collection<br>- Network optimization<br>- Fine-grained parallelism control |
| `parallax-hvm` | 🔴 Early | - Project structure | - HVM integration<br>- HVM parser<br>- Translation layer<br>- HVM optimization |
| `tree-sitter-parallax` | 🟢 Mostly Done | - Complete grammar definition<br>- Syntax parsing | - Fine-tune queries<br>- Language server integration |
| `parallax-cli` | 🔴 Early | - Project structure | - Command implementation<br>- Compiler driver<br>- Build system integration<br>- Error reporting |

### Key Components To Complete

1. **Parser and AST (parallax-lang)**
   - Implement full parsing for all language constructs
   - Comprehensive error recovery and reporting
   - Support for all language features defined in the grammar

2. **Name Resolution (parallax-resolve)**
   - Complete path and import resolution
   - Module system implementation
   - Visibility and privacy checking

3. **Type System (parallax-typeck)**
   - Full implementation of Hindley-Milner type inference
   - Trait system with trait bounds and impl resolution
   - Effect system for tracking side effects
   - Generic type parameter handling

4. **IR Generation (parallax-ir/parallax-mir)**
   - Complete IR definition and generation
   - Mid-level optimizations
   - Control flow analysis
   - Effect annotation

5. **Interaction Net Runtime (parallax-net)**
   - Finish implementation of all reduction rules
   - Optimize parallel execution strategy
   - Implement efficient garbage collection
   - Fine-tune work distribution algorithm

6. **Code Generation (parallax-codegen)**
   - Implement LLVM IR generation
   - Target-specific optimizations
   - Interaction net translation

7. **CLI and Driver (parallax-cli)**
   - Complete compiler driver implementation
   - Project management features
   - Build system integration
   - Package management

### Research Integration Needs

Based on the research directories, the following integration work is needed:

1. **HVM Research**: 
   - Integrate the Higher-order Virtual Machine concepts into parallax-hvm
   - Implement translation between Parallax IR and HVM

2. **Vine Research**:
   - Study the Vine interaction net implementation for parallax-net
   - Adapt the efficient reduction strategies

3. **Hardware Locality**:
   - Apply hardware topology-aware scheduling in parallax-net
   - Optimize work distribution based on NUMA architecture

### Next Steps

1. Focus on completing the core frontend components:
   - Parser and AST (parallax-lang)
   - Name resolution (parallax-resolve)
   - Type system (parallax-typeck)

2. Implement and test the interaction net runtime (parallax-net)

3. Connect the frontend to the backend through IR generation and optimization

4. Build out the codegen infrastructure for LLVM integration

5. Create a comprehensive CLI for end-user usage