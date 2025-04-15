mod broken {
    fn oops(a: i32 -> bool = { // Missing closing parenthesis
        if a > 0 { true } else { false }
    } // Missing semicolon after function body expression?

    struct Bad { x: u32, y: } // Missing type for y
} 