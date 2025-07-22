# Comments

```parallax
// Single-line comment
// Can be placed at the end of a line
let x = 42; // Like this

/* 
   Multi-line comment
   Can span multiple lines
   Useful for longer explanations
*/

/* Can also be used inline */ let y = 10;

/// Documentation comment for the following item
/// Supports markdown formatting
/// 
/// # Example
/// ```parallax
/// let result = documented_function(42);
/// assert_eq!(result, 84);
/// ```
const documented_function = (x: i32) -> i32 => x * 2;

//! Module-level documentation comment
//! Describes the current module
//! 
//! This module provides utilities for...

/** 
 * Alternative documentation style
 * Also supports markdown
 */
type DocumentedType = struct { 
    /// Field documentation
    field: i32 
};
```