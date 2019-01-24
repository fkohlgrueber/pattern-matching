
use syn::{Ident, Result, Token};
use syn::parse::{Parse, ParseStream};
use syn::token;
use syn::{parenthesized, braced};
use syn;
use std::fmt;
use syn::punctuated::Punctuated;

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


enum Expr {
    Node(Ident, Vec<Expr>),
    Ident(Ident),
    Alt(Box<Expr>, Box<Expr>),
    Seq(Box<Expr>, Box<Expr>),
    Repeat(Box<Expr>, RepeatKind),
    Named(Box<Expr>, Ident),
    Expr(syn::Expr),
    Any,
    Empty
}

enum RepeatKind {
    Any,
    Plus,
    Optional,
    Range(Option<syn::LitInt>, Option<syn::LitInt>)
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Node(i, args) => write!(f, "{}({})", i, args.iter().map(|x| format!("{}", x).to_string()).collect::<Vec<_>>().join(", ")),
            Expr::Ident(i) => write!(f, "{}", i),
            Expr::Alt(left, right) => write!(f, "({} | {})", left, right),
            Expr::Seq(left, right) => write!(f, "({}; {})", left, right),
            Expr::Any => write!(f, "_"),
            Expr::Empty => write!(f, "()"),
            Expr::Repeat(e, r) => write!(f, "{}{}", e, match r {
                RepeatKind::Any => "*".to_string(),
                RepeatKind::Plus => "+".to_string(),
                RepeatKind::Optional => "?".to_string(),
                RepeatKind::Range(f, t) => format!("{{{}{}}}", 
                    f.as_ref().map_or("".to_string(), |f| format!("{}", f.value())),
                    t.as_ref().map_or("".to_string(), |t| format!(",{}", t.value()))
                )
            }),
            Expr::Named(e, i) => write!(f, "{}#{}", e, i),
            Expr::Expr(e) => write!(f, "<expr>"),
        }
    }
}

impl Parse for BinOp {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Token![|]) {
            input.parse::<Token![|]>().map(|_| BinOp::Or)
        } else if input.peek(Token![;]) {
            input.parse::<Token![;]>().map(|_| BinOp::Seq)
        } else if !input.is_empty() && !input.peek(Token![,]) {
            Ok(BinOp::Seq)
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

    let mut e = atom_expr(input)?;
    
    if input.peek(Token![*]) {
        input.parse::<Token![*]>()?;
        e = Expr::Repeat(Box::new(e), RepeatKind::Any);
    } else if input.peek(Token![+]) {
        input.parse::<Token![+]>()?;
        e = Expr::Repeat(Box::new(e), RepeatKind::Plus);
    } else if input.peek(Token![?]) {
        input.parse::<Token![?]>()?;
        e = Expr::Repeat(Box::new(e), RepeatKind::Optional);
    } else if input.peek(token::Brace) {
        let content;
        braced!(content in input);
        let f = if content.peek(Token![,]) {
            None
        } else {
            Some(content.parse::<syn::LitInt>()?)
        };
        let t = if content.is_empty() {
            None
        } else {
            content.parse::<Token![,]>()?;
            Some(content.parse::<syn::LitInt>()?)
        };
        e = Expr::Repeat(Box::new(e), RepeatKind::Range(f, t));
    }

    if input.peek(Token![#]) {
        input.parse::<Token![#]>()?;
        e = Expr::Named(Box::new(e), input.parse()?);
    }
    //let mut e = trailer_helper(input, atom)?;

    //Ok(e)
    Ok(e)
}

fn atom_expr(input: ParseStream) -> Result<Expr> {
    if input.peek(Token![_]){
        input.parse::<Token![_]>().map(|_| Expr::Any)
    } else if input.peek(Ident) && input.peek2(token::Paren) {
        let id = input.parse::<Ident>()?;
        let content;
        parenthesized!(content in input);
        let vals: Punctuated<Expr, Token![,]>;
        vals = content.parse_terminated(Expr::parse)?;
        Ok(Expr::Node(
            id,
            vals.into_iter().collect()
        ))
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
        match input.parse::<syn::Expr>() {
            Ok(e) => Ok(Expr::Expr(e)),
            Err(_) => Err(input.error("unsupported expression; enable syn's features=[\"full\"]"))
        }
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
        let res = syn::parse_str::<Expr>("Foo(x, 1* (z)){,2}#xy Foo() | x y");

        match res {
            Ok(i) => println!("{}", i),
            Err(e) => println!("{:?}", e),
        }
    }

    #[test]
    fn answer_test() {

    }

}
