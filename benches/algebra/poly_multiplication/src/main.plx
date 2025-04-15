// Polynomial representation as a list of coefficients
// Each element in the list represents a coefficient, starting from the constant term
pub type Poly = List<i32>;

// Adds two polynomials term by term
// Example: (1 + 2x) + (3 + 4x) = (1+3) + (2+4)x = 4 + 6x
fn poly_add(p1: Poly, p2: Poly) -> Poly = match (p1, p2) {
    (Nil, p2) => p2,                                      // If p1 is empty, return p2
    (p1, Nil) => p1,                                      // If p2 is empty, return p1
    (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, poly_add(t1, t2)), // Add corresponding coefficients
};

// Multiplies two polynomials using the distributive property
// Example: (1 + 2x) * (3 + 4x) = 3 + 4x + 6x + 8x² = 3 + 10x + 8x²
fn poly_mul(p1: Poly, p2: Poly) -> Poly = match (p1, p2) {
    (Nil, _) => Nil,                                      // If p1 is empty, result is empty
    (_, Nil) => Nil,                                      // If p2 is empty, result is empty
    (Cons(h1, t1), _) => poly_add(
        scalar_mul(p2, h1),                              // Multiply p2 by the first coefficient of p1
        shift(poly_mul(t1, p2), 1)                       // Recursively multiply rest of p1 with p2 and shift
    ),
};

// Multiplies a polynomial by a scalar value
// Example: 2 * (3 + 4x) = 6 + 8x
fn scalar_mul(p: Poly, scalar: i32) -> Poly = match p {
    Nil => Nil,                                           // Empty polynomial remains empty
    Cons(head, tail) => Cons(head * scalar, scalar_mul(tail, scalar)), // Multiply each coefficient by scalar
};

// Shifts a polynomial by n positions (multiplies by x^n)
// Example: shift([1, 2], 1) = [0, 1, 2] (x * (1 + 2x) = x + 2x²)
fn shift(p: Poly, n: i32) -> Poly = if n == 0 then p else Cons(0, shift(p, n - 1));


// Example: (1 + 2x) * (3 + 4x) = 3 + 10x + 8x²
fn main() -> Poly = poly_mul(Cons(1, Cons(2, Nil)), Cons(3, Cons(4, Nil)));




fn hi() -> fn(i32) -> i32 = {
    let x = rand(0, 10);
    if x > 5 then |y| => y + x else |y| => y + 1
};

