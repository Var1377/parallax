# Types

## Basic Types

```parallax
// Primitives
i8, i16, i32, i64, i128    // Signed integers
u8, u16, u32, u64, u128    // Unsigned integers
f32, f64                    // Floating point
bool                        // Boolean
char                        // Unicode character
str                         // String slice

// Compound types
[T; N]                      // Array of N elements
(T, U, V)                   // Tuple
&T                          // Immutable reference
&mut T                      // Mutable reference
```

## Custom Types

```parallax
// Type alias
type NodeId = u64;
type Point2D = (f64, f64);

// Struct with named fields
type Point = struct {
    x: f64,
    y: f64,
};

// Tuple struct (positional fields)
type Color = struct(u8, u8, u8);

// Enum (algebraic data type)
type Option = [T] => enum {
    Some(T),
    None,
};

// Complex enum
type Result = [T, E] => enum {
    Ok(T),
    Err(E),
};

// Type with behavior enhancement
type User = struct {
    name: String,
    email: String,
}.with(UserBehavior);

// Type-level functions
type SafeArray = [T, N: usize] => {
    static_assert!(N <= 1024, "Array too large");
    struct { data: [T; N] }
};
```