pub fn println(s: string) -> () = __intrinsic_println__(s); // Intrinsic implementation, marked as effectful
pub fn readln() -> string = __intrinsic_readln__(); // Intrinsic implementation, marked as effectful

fn __intrinsic_println__(s: string) -> ();
fn __intrinsic_readln__() -> string;