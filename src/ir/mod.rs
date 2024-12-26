mod parser;
mod lexer;
mod ast;

#[cfg(test)]
mod tests {
    use chumsky::Parser;

    use super::{lexer::*, parser::*};

    #[test]
    fn sum_rec() {
        let input = r#"@main = a
        & @sum ~ (20 (0 a))

        @sum = (?(((a a) @sum__C0) b) b)

        @sum__C0 = ({c a} ({$([*2] $([+1] d)) $([*2] $([+0] b))} f))
        &! @sum ~ (a (b $([+] $(e f))))
        &! @sum ~ (c (d e))
        "#;

        let (_str, tokens) = lex(&input).expect("Lexing failed");
        let book = parser().parse(tokens).expect("Parsing failed");

        println!("{:#?}", book);
    }

    #[test]
    fn sum_tree() {
        let input = r#"@gen = (?(((a a) @gen__C0) b) b)
        @gen__C0 = ({a d} ({$([*2] $([+1] b)) $([*2] e)} (c f)))
        &! @gen ~ (a (b c))
        &! @gen ~ (d (e f))

        @main = a
        & @sum ~ (20 (@main__C0 a))

        @main__C0 = a
        & @gen ~ (20 (0 a))

        @sum = (?(((* 1) @sum__C0) a) a)

        @sum__C0 = ({a c} ((b d) f))
        &! @sum ~ (a (b $([+] $(e f))))
        &! @sum ~ (c (d e))
        "#;

        let (_str, tokens) = lex(input).expect("Lexing failed");
        let book = parser().parse(tokens).expect("Parsing failed");

        println!("{:#?}", book);
    }
}