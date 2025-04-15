pub enum Expr {
    Var(string),
    Const(f64),
    Add(Expr, Expr),
    Sub(Expr, Expr),
    Mul(Expr, Expr),
    Div(Expr, Expr),
    Pow(Expr, Expr),
}

fn diff(expr: Expr, var: string) -> Expr = match expr {
    Expr::Var(x) => if x == var then Expr::Const(1.0) else Expr::Const(0.0),
    Expr::Const(_) => Expr::Const(0.0),
    Expr::Add(e1, e2) => Expr::Add(diff(e1, var), diff(e2, var)),
    Expr::Sub(e1, e2) => Expr::Sub(diff(e1, var), diff(e2, var)),
    Expr::Mul(e1, e2) => Expr::Add(Expr::Mul(diff(e1, var), e2), Expr::Mul(e1, diff(e2, var))),
    Expr::Div(e1, e2) => Expr::Div(Expr::Sub(Expr::Mul(diff(e1, var), e2), Expr::Mul(e1, diff(e2, var))), Expr::Mul(e2, e2)),
    Expr::Pow(base, exp) => Expr::Mul(Expr::Mul(exp, Expr::Pow(base, Expr::Sub(exp, Expr::Const(1.0)))), diff(base, var)),
};

fn main() -> Expr = diff(Expr::Mul(Expr::Var("x"), Expr::Var("y")), "x");
