fn match_test(value: Option<(i32, Point)>) = {
    match value {
        Some((x, Point { y, .. })) => println("X and Y extracted"), // Removed guard
        Some((0 | 1, _)) => println("Zero or One"), // Or pattern, wildcard
        Some((z, p)) => println("Specific tuple {} at {:?}", z, p), // Removed range/binding
        None => println("None"),
        _ => println("Other Some"), // This might become unreachable depending on above changes
    }
};

fn let_patterns() = {
    let Point { x, y: y_val } = get_point(); // Struct pattern rename
    let (first, second, third) = get_tuple(); // Fixed size tuple
    let [a, b] = get_array(); // Fixed size array
    let Ok(value) = get_result(); // Constructor pattern
};

struct Point { x: i32, y: i32 } 