// tests/patterns/mod.rs
// Declare sub-modules for pattern matching tests

mod basic;
mod tuple;
mod r#struct; // Use raw identifier for struct
mod r#enum;   // Use raw identifier for enum
mod array;
mod or_pattern; // Avoid clash with `or` keyword
mod exhaustiveness; // Added exhaustiveness module

// Example:
// mod basic_patterns;
// mod struct_patterns;
// mod enum_patterns; 