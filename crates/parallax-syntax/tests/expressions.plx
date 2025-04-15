fn operators() = {
    let a = 1 + 2 * 3 - 4 / 2; // Arithmetic precedence
    let b = true && !false || false;
    let c = (a > 5) == b;
    let d = -a;
    let e = 10;
    let f = 20;
    let g = a | 1;
    let h = a & 0xF0;
    let i = a << 2;
    let j = a >> 1;
    let k = a;
    let m = a + 1;
    let n = [1, 2, 3];
    let p = (1, "hello");
    let u = Obj { field: 10 };
    let v = u.field;
    let w = if c then { a } else { f };
    let x = match a {
        0 => "zero",
        1 | 2 | 3 | 4 | 5 => "small",
        _ => "large",
    };
    let y = { let inner = 1; inner + 1 }; // Block expression

};

struct Obj {
    field: i32
}

fn complex_call() = {
    let data = 42;
    let config = Config { retries: 3 };
    let flags = [Flag::A, Flag::B];
    process(data, config, flags)
};

// Supporting types needed for the functions above
struct Config {
    retries: u32
}

enum Flag {
    A,
    B
}

fn process(data: u32, config: Config, flags: u32) -> u32 = 0;