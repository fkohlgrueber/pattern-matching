
use syn::{Ident, Result, Token};
use syn::parse::{Parse, ParseStream};
use syn::token;
use syn::parenthesized;
use syn;
use std::fmt;

#[derive(Debug)]
enum BinOp {
    Or,
    Seq
}

#[derive(Copy, Clone, PartialEq, PartialOrd)]
enum Precedence {
    Any,
    Or,
    And,
}

impl Precedence {
    fn of(op: &BinOp) -> Self {
        match *op {
            BinOp::Seq => Precedence::And,
            BinOp::Or => Precedence::Or,
        }
    }
}

#[derive(Debug)]
enum Expr {
    Ident(Ident),
    Alt(Box<Expr>, Box<Expr>),
    Seq(Box<Expr>, Box<Expr>),
    Any,
    Empty
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Ident(i) => write!(f, "{}", i),
            Expr::Alt(left, right) => write!(f, "({} | {})", left, right),
            Expr::Seq(left, right) => write!(f, "({}; {})", left, right),
            Expr::Any => write!(f, "_"),
            Expr::Empty => write!(f, "()")
        }
    }
}

impl Parse for BinOp {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Token![|]) {
            input.parse::<Token![|]>().map(|_| BinOp::Or)
        } else if input.peek(Token![;]) {
            input.parse::<Token![;]>().map(|_| BinOp::Seq)
        } else {
            Err(input.error("expected binary operator"))
        }
    }
}

impl Parse for Expr {
    fn parse(input: ParseStream) -> Result<Self> {
        ambiguous_expr(input)
    }
}

// Parse an arbitrary expression.
fn ambiguous_expr(input: ParseStream) -> Result<Expr> {
    let lhs = unary_expr(input)?;
    parse_expr(input, lhs, Precedence::Any)
}

fn unary_expr(input: ParseStream) -> Result<Expr> {
    trailer_expr(input)
}

fn trailer_expr(input: ParseStream) -> Result<Expr> {
    /*if input.peek(token::Group) {
        return input.call(expr_group).map(Expr::Group);
    }*/

    let atom = atom_expr(input)?;
    //let mut e = trailer_helper(input, atom)?;

    //Ok(e)
    Ok(atom)
}

fn atom_expr(input: ParseStream) -> Result<Expr> {
    if input.peek(Token![_]){
        input.parse::<Token![_]>().map(|_| Expr::Any)
    } else if input.peek(Ident) {
        input.parse().map(Expr::Ident)
    } else if input.peek(token::Paren) {
        let content;
        parenthesized!(content in input);
        if content.is_empty() {
            return Ok(Expr::Empty);
        }
        let first: Expr = content.parse()?;
        Ok(first)
    } else {
        Err(input.error("unsupported expression; enable syn's features=[\"full\"]"))
    }
}

fn parse_expr(
    input: ParseStream,
    mut lhs: Expr,
    base: Precedence,
) -> Result<Expr> {
    loop {
        if input
            .fork()
            .parse::<BinOp>()
            .ok()
            .map_or(false, |op| Precedence::of(&op) >= base)
        {
            let op: BinOp = input.parse()?;
            let precedence = Precedence::of(&op);
            let mut rhs = unary_expr(input)?;
            loop {
                let next = peek_precedence(input);
                if next > precedence {
                    rhs = parse_expr(input, rhs, next)?;
                } else {
                    break;
                }
            }
            lhs = match op {
                BinOp::Seq => Expr::Seq(Box::new(lhs), Box::new(rhs)),
                BinOp::Or => Expr::Alt(Box::new(lhs), Box::new(rhs))
            }
        } else {
            break;
        }
    }
    Ok(lhs)
}

fn peek_precedence(input: ParseStream) -> Precedence {
    if let Ok(op) = input.fork().parse() {
        Precedence::of(&op)
    } else {
        Precedence::Any
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let res = syn::parse_str::<Expr>("(a | _) ; () | c");

        match res {
            Ok(i) => println!("{}", i),
            Err(e) => println!("{:?}", e),
        }
    }

    #[test]
    fn answer_test() {

    }

}
