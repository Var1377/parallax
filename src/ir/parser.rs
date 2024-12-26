use super::{ast::*, lexer::Token};
use chumsky::prelude::*;

pub fn parser<'a>() -> impl Parser<Token<'a>, Book<'a>, Error = Simple<Token<'a>>> {
    let ws = || {
        filter::<Token<'a>, _, Simple<Token<'a>>>(|t| matches!(t, Token::Whitespace(_)))
            .repeated()
            .ignored()
    };

    let identifier = select! {
        Token::Identifier(id) => id
    }
    .labelled("identifier");

    let number = just(Token::Plus)
        .or(just(Token::Minus))
        .then_ignore(ws())
        .or_not()
        .then(select! {
            Token::Number(n) => n
        })
        .map(|(sign, digits)| {
            let sign_str = match sign {
                Some(Token::Plus) => "+",
                Some(Token::Minus) => "-",
                _ => "",
            };
            let full_str = format!("{}{}", sign_str, digits);
            if full_str.contains('.') {
                Number::Float(full_str.parse().unwrap())
            } else {
                Number::Int(full_str.parse().unwrap())
            }
        })
        .labelled("number");

    let operation = select! {
        Token::Plus => Operation::Add,
        Token::Minus => Operation::Sub,
        Token::Star => Operation::Mul,
        Token::Slash => Operation::Div,
        Token::Percent => Operation::Mod,
        Token::Equals => Operation::Eq,
        Token::Bang => Operation::Neq,
        Token::Lt => Operation::Lt,
        Token::Gt => Operation::Gt,
        Token::Le => Operation::Le,
        Token::Ge => Operation::Ge,
        Token::Ampersand => Operation::And,
        Token::Pipe => Operation::Or,
        Token::Caret => Operation::Xor,
        Token::Shr => Operation::Shr,
        Token::Shl => Operation::Shl,
    }
    .labelled("operation");

    let numeric = choice((
        ws().ignore_then(operation)
            .then_ignore(ws())
            .then(number.clone().or_not())
            .then_ignore(ws())
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map(|(op, num)| Numeric::Operator(op, num)),
        number.clone().map(Numeric::Number),
    ));

    let tree = recursive(|tree| {
        choice((
            identifier.clone().map(Tree::Variable),
            just(Token::At)
                .ignore_then(identifier.clone())
                .map(Tree::Reference),
            just(Token::Star).to(Tree::Eraser),
            ws().ignore_then(tree.clone())
                .then_ignore(ws())
                .then(tree.clone())
                .then_ignore(ws())
                .delimited_by(just(Token::LParen), just(Token::RParen))
                .map(|(lhs, rhs)| Tree::Constructor(Box::new(lhs), Box::new(rhs))),
            ws().ignore_then(tree.clone())
                .then_ignore(ws())
                .then(tree.clone().then_ignore(ws()))
                .delimited_by(just(Token::LBrace), just(Token::RBrace))
                .map(|(lhs, rhs)| Tree::Duplicator(Box::new(lhs), Box::new(rhs))),
            ws().ignore_then(tree.clone())
                .then_ignore(ws())
                .then(tree.clone())
                .then_ignore(ws())
                .delimited_by(just(Token::DollarParen), just(Token::RParen))
                .map(|(lhs, rhs)| Tree::Operator(Box::new(lhs), Box::new(rhs))),
            ws().ignore_then(tree.clone())
                .then_ignore(ws())
                .then(tree.clone())
                .then_ignore(ws())
                .delimited_by(just(Token::QuestionParen), just(Token::RParen))
                .map(|(lhs, rhs)| Tree::Switch(Box::new(lhs), Box::new(rhs))),
            numeric.map(Tree::Numeric),
        ))
    });

    let net = tree
        .clone()
        .then(
            ws().ignore_then(just(Token::Ampersand))
                .ignore_then(ws())
                .ignore_then(just(Token::Bang).or_not().map(|b| b.is_some()))
                .then_ignore(ws())
                .then(tree.clone())
                .then_ignore(ws())
                .then_ignore(just(Token::Tilde))
                .then_ignore(ws())
                .then(tree.clone())
                .map(|((b, lhs), rhs)| Redex(b, lhs, rhs))
                .repeated(),
        )
        .map(|(tree, redexes)| Network(tree, redexes));

    let named_net = just(Token::At)
        .ignore_then(identifier.clone())
        .then_ignore(ws())
        .then_ignore(just(Token::Equals))
        .then_ignore(ws())
        .then(net.clone())
        .map(|(ident, net)| NamedNetwork(ident, net));

    ws().ignore_then(named_net)
        .repeated()
        .map(Book)
        .then_ignore(ws())
        .then_ignore(end())
        .boxed()
}
