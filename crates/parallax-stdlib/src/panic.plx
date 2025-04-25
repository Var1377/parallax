pub fn panic(message: string) -> ! = __intrinsic_panic__(message);

fn __intrinsic_panic__(message: string) -> !;
