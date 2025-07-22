# Modules and Visibility

```parallax
// Module declaration
mod utils {
    // Public function - visible outside module
    pub const helper = (x: i32) -> i32 => x * 2;
    
    // Private by default - only visible within module
    const internal = () => "secret";
    
    // Nested modules
    pub mod math {
        pub const add = (a: i32, b: i32) -> i32 => a + b;
        pub const PI = 3.14159;
    }
}

// Using items from modules
let result = utils::helper(21);
let sum = utils::math::add(1, 2);

// Imports
import std::collections::{Vec, HashMap};
import std::io::Result;
import utils::helper;
import utils::math::{add, PI};

// Import with alias
import std::collections::HashMap as Map;

// Import all public items
import utils::math::*;

// Re-exports
export const api_function = utils::helper;
export type PublicType = Point;

// Visibility modifiers
pub const PUBLIC_CONSTANT = 42;
pub(crate) const CRATE_VISIBLE = "internal";
pub(super) const PARENT_VISIBLE = true;
pub(in path::to::module) const PATH_VISIBLE = 0;
```