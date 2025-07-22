# Basic Values

Parallax provides a rich set of literal values and binding mechanisms that form the foundation of all programs.

## Literals

### Numeric Literals

Parallax supports various numeric types with flexible literal syntax:

```parallax
// Integers default to i32
42          // i32 by default
-17         // Negative i32

// Type suffixes for explicit typing
42u64       // Unsigned 64-bit integer
100i8       // Signed 8-bit integer
3.14f32     // 32-bit floating point
2.71828f64  // 64-bit floating point (default for decimals)

// Alternative bases for integers
0xFF        // Hexadecimal (255)
0o77        // Octal (63)
0b1010      // Binary (10)

// Underscores for readability
1_000_000   // One million
0xFF_FF_FF  // RGB white
0b1111_0000 // Binary with grouping
```

The type suffix system allows precise control over numeric types without verbose type annotations. Underscores can be inserted anywhere within numeric literals to improve readability, especially for large numbers or when grouping bits.

### String and Character Literals

Parallax distinguishes between borrowed and owned strings at the syntax level:

```parallax
// Basic string literals
"Hello"     // &str - borrowed string slice
s"Hello"    // String - owned string, allocated on heap
'H'         // char - single Unicode character (4 bytes)

// String prefixes modify behavior
r"C:\path\to\file"     // Raw string - no escape sequences
rs"C:\path\to\file"    // Owned raw string
f"Hello, {name}!"      // Format string with interpolation (owned)
rf"{path}\n"           // Raw format string (no escapes, with interpolation)
```

#### String Interpolation

Format strings use curly braces for interpolation:

```parallax
let name = "Alice";
let age = 30;
let message = f"Hello, {name}! You are {age} years old.";
// Expressions are allowed
let calc = f"2 + 2 = {2 + 2}";
```

#### Multi-line Strings

All string types support multiple lines naturally:

```parallax
let poem = "Roses are red
Violets are blue
Rust is fast
And Parallax is too";

let raw_text = r"This contains
literal \n characters
not newlines";
```

#### Raw String Delimiters

For strings containing quotes, use flexible delimiter syntax:

```parallax
r#"He said "hello" to me"#        // One # delimiter
r##"This has "# in it"##          // Two ## delimiters
r###"Even "##" works"###          // Three ### delimiters
```

### Boolean and Unit Literals

```parallax
true        // Boolean true
false       // Boolean false
()          // Unit type and value - represents "nothing"
```

The unit type `()` is particularly important in Parallax as it represents computations that don't return a meaningful value, similar to `void` in other languages but as a real value that can be passed around.

## Bindings

Bindings in Parallax associate names with values. The language emphasizes immutability by default with explicit opt-in for mutation.

### Basic Bindings

```parallax
// Immutable binding (default)
let x = 42;
// x = 43;  // ERROR: Cannot mutate immutable binding

// Mutable binding requires explicit 'mut'
let mut counter = 0;
counter += 1;  // OK: mutable binding can be modified

// Type annotations are optional but can be explicit
let explicit: i32 = 42;
let inferred = 42;  // Type inferred as i32
```

### Compile-Time Constants

Constants are evaluated at compile time and can be used in type-level contexts:

```parallax
// Constants must have explicit types
const PI: f64 = 3.14159265359;
const MAX_SIZE: usize = 1024;

// Constants can be used in types
type Buffer = [u8; MAX_SIZE];

// Constants are inlined at usage sites
let circumference = 2.0 * PI * radius;
```

### Pattern Matching in Bindings

Parallax supports destructuring patterns directly in let bindings:

```parallax
// Tuple destructuring
let (x, y) = (10, 20);
let (first, second, third) = (1, "hello", true);

// Struct destructuring
let Point { x, y } = get_point();
let Point { x: px, y: py } = point;  // Rename during destructuring

// Array/Vector destructuring with spread
let [head, ...tail] = vec;
let [first, second, ...rest] = [1, 2, 3, 4, 5];
let [...init, last] = list;

// Nested patterns
let ((a, b), Point { x, y }) = complex_value;

// Ignoring values with underscore
let (important, _) = get_pair();
let [_, _, third, ..] = array;
```

### Shadowing

Bindings can shadow previous bindings with the same name:

```parallax
let x = 5;
let x = x + 1;      // New binding shadows the previous one
let x = x * 2;      // Another shadow
let x = "hello";    // Can even change types

// Each 'let' creates a new binding
// Previous bindings are not mutated
```

Shadowing is particularly useful for transforming values through a series of steps without needing mutable bindings or coming up with new names.

### Binding Scope

Bindings are scoped to their enclosing block:

```parallax
let outer = 1;
{
    let inner = 2;
    let outer = 3;  // Shadows outer binding within this block
    // Both inner and shadowed outer are accessible here
}
// inner is no longer accessible
// outer is back to 1
```

## Type Inference

Parallax uses bidirectional type inference, flowing type information both from definitions to uses and from uses back to definitions:

```parallax
let numbers = @[1, 2, 3];  // Inferred as Vec[i32]
let doubled = numbers.map(x => x * 2);  // x inferred as i32

// Type flows from usage
let mut result = @[];
result.push(42);  // result inferred as Vec[i32]

// Explicit type when needed
let empty = @[] : Vec[String];
```

This approach minimizes the need for type annotations while maintaining full type safety and enabling powerful type-level features.