# Expressions

In Parallax, everything is an expression that produces a value. This fundamental principle eliminates the distinction between statements and expressions, making the language more composable and predictable.

## Expression-Oriented Design

Unlike statement-based languages where some constructs perform actions without returning values, every Parallax construct evaluates to a value:

```parallax
// Block expression returns its last value
let result = {
    let temp = compute();
    temp * 2  // No semicolon - this is the block's value
};

// If expression returns a value
let max = if a > b then a else b;

// Match expression returns a value
let description = match value with
    0 => "zero"
    | 1 => "one"
    | n if n < 0 => "negative"
    | _ => "positive";
```

This design enables powerful composition patterns and eliminates special cases in the language grammar.

## Operators

Parallax provides a comprehensive set of operators that work uniformly across numeric types:

### Arithmetic Operators

```parallax
let sum = a + b;         // Addition
let diff = a - b;        // Subtraction
let product = a * b;     // Multiplication
let quotient = a / b;    // Division
let remainder = a % b;   // Remainder/modulo
let power = a ** b;      // Exponentiation

// Operators work with type inference
let x = 10;              // Inferred as i32
let y = x * 2;           // Also i32
let z = x as f64 * 2.5;  // Explicit conversion needed
```

### Comparison Operators

All comparison operators return `bool`:

```parallax
let equal = x == y;      // Equality
let not_equal = x != y;  // Inequality
let less = x < y;        // Less than
let less_eq = x <= y;    // Less than or equal
let greater = x > y;     // Greater than
let greater_eq = x >= y; // Greater than or equal

// Chained comparisons (evaluated left-to-right)
let in_range = 0 <= x && x < 100;
```

### Logical Operators

Logical operators use short-circuit evaluation:

```parallax
let both = p && q;       // Logical AND - q not evaluated if p is false
let either = p || q;     // Logical OR - q not evaluated if p is true
let negated = !p;        // Logical NOT

// Short-circuit evaluation is guaranteed
let safe = ptr != null && ptr.value > 0;  // Safe: won't deref null
```

### Bitwise Operators

For integer types:

```parallax
let and_bits = a & b;    // Bitwise AND
let or_bits = a | b;     // Bitwise OR
let xor_bits = a ^ b;    // Bitwise XOR
let not_bits = ~a;       // Bitwise NOT
let left = a << n;       // Left shift
let right = a >> n;      // Arithmetic right shift (sign-extending)
let logical = a >>> n;   // Logical right shift (zero-filling)
```

### Special Operators

```parallax
// Range operators create range values
let exclusive = 1..10;   // 1 through 9
let inclusive = 1..=10;  // 1 through 10

// Error propagation operator
let result = risky_operation()?;  // Returns early if error

// Pipeline operator for readability
let processed = data 
    |> validate 
    |> transform 
    |> serialize;
// Equivalent to: serialize(transform(validate(data)))
```

## Control Flow

All control flow constructs are expressions that return values.

### Conditional Expressions

The if-then-else expression evaluates to the value of the chosen branch:

```parallax
// Simple conditional
let status = if ready then "go" else "wait";

// Multi-branch conditional
let grade = if score >= 90 then "A"
           else if score >= 80 then "B"
           else if score >= 70 then "C"
           else if score >= 60 then "D"
           else "F";

// Blocks in branches
let result = if condition then {
    log("Taking true branch");
    expensive_computation()
} else {
    log("Taking false branch");
    default_value()
};
```

Both branches must have compatible types. The compiler ensures exhaustiveness.

### Guard Patterns

Guard patterns provide early-return syntax for pattern matching:

```parallax
// Early return if pattern doesn't match
let Some(value) = maybe_value else return None;
// Continues here only if maybe_value was Some

// Can be used with any pattern
let Ok(data) = parse_input() else return Err("Invalid input");
let [first, ...rest] = list else return handle_empty();
```

Guards are particularly useful for reducing nesting in error handling code.

### Pattern Matching

Match expressions provide exhaustive pattern matching with powerful binding capabilities:

```parallax
match value with
    // Literal patterns
    0 => "exactly zero"
    | 1 => "exactly one"
    
    // Variable binding captures the value
    | n => f"the number {n}"
    
    // Struct destructuring
    | Point { x: 0, y } => f"on y-axis at {y}"
    | Point { x, y: 0 } => f"on x-axis at {x}"
    | Point { x, y } => f"at ({x}, {y})"
    
    // Enum matching
    | Some(inner) => f"contains {inner}"
    | None => "empty"
    
    // Guards add conditions to patterns
    | n if n > 0 => "positive"
    | n if n < 0 => "negative"
    
    // Range patterns
    | 10..=20 => "teen"
    | 21..=99 => "adult"
    
    // Wildcard matches anything
    | _ => "something else"
```

The compiler ensures all patterns are exhaustive and warns about unreachable patterns.

## Iteration

Parallax eschews traditional loops in favor of iterator methods that compose naturally as expressions:

### Iterator Methods

```parallax
// Basic iteration with foreach (for side effects)
(0..10).foreach(i => println!("Number: {}", i));

// Transformation pipelines
let result = vec.iter()
    .filter(x => x > 0)        // Keep positive numbers
    .map(x => x * 2)           // Double them
    .take(5)                   // Take first 5
    .collect[Vec[_]]();        // Collect into vector

// Folding (reduction)
let sum = numbers.fold(0, (acc, x) => acc + x);
let product = numbers.fold(1, (acc, x) => acc * x);

// While-like iteration
let result = iterate_while(
    0,                         // Initial value
    x => x < 100,             // Continue condition
    x => x + fibonacci(x)      // Next value function
);
```

### Why No Loops?

The absence of traditional loops has several benefits:

1. **Composability**: Iterator methods chain naturally
2. **Parallelism**: Pure iteration can be automatically parallelized
3. **Safety**: No off-by-one errors or infinite loops
4. **Expressiveness**: Intent is clearer with named operations

## Block Expressions

Blocks create new scopes and evaluate to their final expression:

```parallax
let value = {
    let a = compute_a();
    let b = compute_b();
    // Semicolon would make this statement, not expression
    a + b  // Block's value
};

// Empty blocks evaluate to unit
let unit_value = { };  // Type: ()

// Blocks can contain any expressions
let complex = {
    if condition then process_true() else process_false()
};
```

## Operator Precedence

Operators follow standard mathematical precedence:

1. Function calls, field access, array indexing
2. Unary operators (`!`, `~`, `-`)
3. Exponentiation (`**`)
4. Multiplicative (`*`, `/`, `%`)
5. Additive (`+`, `-`)
6. Shifts (`<<`, `>>`, `>>>`)
7. Bitwise AND (`&`)
8. Bitwise XOR (`^`)
9. Bitwise OR (`|`)
10. Comparisons (`<`, `>`, `<=`, `>=`, `==`, `!=`)
11. Logical AND (`&&`)
12. Logical OR (`||`)
13. Range (`..`, `..=`)
14. Assignment (`=`, `+=`, etc.)

When in doubt, use parentheses for clarity:

```parallax
// Clear precedence
let result = (a + b) * c;
let flag = (x > 0) && (y < 10);
```