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


#[derive(PartialEq, Debug)]
pub enum Expr {
    Node(Ident, Vec<Expr>),
    Alt(Box<Expr>, Box<Expr>),
    Seq(Box<Expr>, Box<Expr>),
    Repeat(Box<Expr>, RepeatKind),
    Named(Box<Expr>, Ident),
    Lit(syn::Lit),
    Any,
    Empty // matches the empty sequence `()`
}

#[derive(PartialEq, Debug)]
pub enum RepeatKind {
    Any,
    Plus,
    Optional,
    Range(syn::LitInt, Option<syn::LitInt>),
    Repeat(syn::LitInt)
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Node(i, args) => write!(f, "{}({})", i, args.iter().map(|x| format!("{}", x).to_string()).collect::<Vec<_>>().join(", ")),
            Expr::Alt(left, right) => write!(f, "({} | {})", left, right),
            Expr::Seq(left, right) => write!(f, "({}; {})", left, right),
            Expr::Any => write!(f, "_"),
            Expr::Empty => write!(f, "()"),
            Expr::Repeat(e, r) => write!(f, "{}{}", e, match r {
                RepeatKind::Any => "*".to_string(),
                RepeatKind::Plus => "+".to_string(),
                RepeatKind::Optional => "?".to_string(),
                RepeatKind::Range(f, t) => format!("{{{}{}}}", 
                    f.value(),
                    t.as_ref().map_or("".to_string(), |t| format!(",{}", t.value()))
                ),
                RepeatKind::Repeat(r) => format!("{{{}}}", r.value())
            }),
            Expr::Named(e, i) => write!(f, "{}#{}", e, i),
            Expr::Lit(_e) => write!(f, "<expr>"),
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
        let f = content.parse::<syn::LitInt>()?;
        if content.is_empty() {
            e = Expr::Repeat(Box::new(e), RepeatKind::Repeat(f))
        } else {
            content.parse::<Token![,]>()?;
            let t = if content.is_empty() {
                None
            } else {
                Some(content.parse::<syn::LitInt>()?)
            };
            e = Expr::Repeat(Box::new(e), RepeatKind::Range(f, t));
        };
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
    /*} else if input.peek(Ident) {
        input.parse().map(Expr::Ident)*/
    } else if input.peek(token::Paren) {
        let content;
        parenthesized!(content in input);
        if content.is_empty() {
            return Ok(Expr::Empty);
        }
        let first: Expr = content.parse()?;
        Ok(first)
    } else {
        match input.parse::<syn::Lit>() {
            Ok(e) => Ok(Expr::Lit(e)),
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

    fn parse_test(input: &'static str, exp: Expr) {
        let res = syn::parse_str::<Expr>(input);
        assert_eq!(res.ok(), Some(exp));
    }

    fn int_lit(input: u64) -> syn::LitInt {
        syn::LitInt::new(input, syn::IntSuffix::None, proc_macro2::Span::call_site())
    }

    #[test]
    fn any() {
        parse_test("_", Expr::Any);
    }

    #[test]
    fn empty() {
        parse_test(
            "()",
            Expr::Empty
        )
    }

    #[test]
    fn lit() {
        let exp = Expr::Lit(
            syn::Lit::Int(int_lit(123))
        );
        parse_test("123", exp);

        let exp = Expr::Lit(
            syn::Lit::Bool(
                syn::LitBool {
                    value: false,
                    span: proc_macro2::Span::call_site()
                }
            )
        );
        parse_test("false", exp);
    }

    #[test]
    fn named() {
        parse_test(
            "_#test", 
            Expr::Named(
                Box::new(Expr::Any), 
                proc_macro2::Ident::new("test", proc_macro2::Span::call_site())
            )
        );
    }

    #[test]
    fn repeat() {
        parse_test(
            "_*", 
            Expr::Repeat(
                Box::new(Expr::Any), 
                RepeatKind::Any
            )
        );
        parse_test(
            "_+", 
            Expr::Repeat(
                Box::new(Expr::Any), 
                RepeatKind::Plus
            )
        );
        parse_test(
            "_?", 
            Expr::Repeat(
                Box::new(Expr::Any), 
                RepeatKind::Optional
            )
        );
        parse_test(
            "_{3, 4}", 
            Expr::Repeat(
                Box::new(Expr::Any), 
                RepeatKind::Range(
                    int_lit(3),
                    Some(int_lit(4)),
                )
            )
        );
        parse_test(
            "_{3, }", 
            Expr::Repeat(
                Box::new(Expr::Any), 
                RepeatKind::Range(
                    int_lit(3),
                    None,
                )
            )
        );
        parse_test(
            "_{3}", 
            Expr::Repeat(
                Box::new(Expr::Any), 
                RepeatKind::Repeat(
                    int_lit(3)
                )
            )
        );
    }


    #[test]
    fn seq() {
        parse_test(
            "_ 1", 
            Expr::Seq(
                Box::new(Expr::Any), 
                Box::new(Expr::Lit(syn::Lit::Int(int_lit(1))))
            )
        );
        parse_test(
            "1 2 3", 
            Expr::Seq(
                Box::new(Expr::Seq(
                    Box::new(Expr::Lit(syn::Lit::Int(int_lit(1)))),
                    Box::new(Expr::Lit(syn::Lit::Int(int_lit(2))))
                )),
                Box::new(Expr::Lit(syn::Lit::Int(int_lit(3))))
            )
        );
    }


    #[test]
    fn alt() {
        parse_test(
            "_ | 1", 
            Expr::Alt(
                Box::new(Expr::Any), 
                Box::new(Expr::Lit(syn::Lit::Int(int_lit(1))))
            )
        );
        parse_test(
            "1|2|3",
            Expr::Alt(
                Box::new(Expr::Alt(
                    Box::new(Expr::Lit(syn::Lit::Int(int_lit(1)))),
                    Box::new(Expr::Lit(syn::Lit::Int(int_lit(2))))
                )),
                Box::new(Expr::Lit(syn::Lit::Int(int_lit(3))))
            )
        );
    }

    #[test]
    fn node() {
        parse_test(
            "Test()", 
            Expr::Node(
                proc_macro2::Ident::new("Test", proc_macro2::Span::call_site()),
                vec!()
            )
        );
        parse_test(
            "Test(1)", 
            Expr::Node(
                proc_macro2::Ident::new("Test", proc_macro2::Span::call_site()),
                vec!(
                    Expr::Lit(syn::Lit::Int(int_lit(1)))
                )
            )
        );
        parse_test(
            "Test(1,2)", 
            Expr::Node(
                proc_macro2::Ident::new("Test", proc_macro2::Span::call_site()),
                vec!(
                    Expr::Lit(syn::Lit::Int(int_lit(1))),
                    Expr::Lit(syn::Lit::Int(int_lit(2)))
                )
            )
        );
    }

    #[test]
    fn precedence() {
        // alt < seq
        parse_test(
            "_ _ | _",  // ( _ _ ) | ( _ )
            Expr::Alt(
                Box::new(Expr::Seq(
                    Box::new(Expr::Any),
                    Box::new(Expr::Any)
                )),
                Box::new(Expr::Any)
            )
        );
        // alt < seq < named
        parse_test(
            "_ | _ _ #name",  // ( _ ) | ( _ ( _ #name ) )
            Expr::Alt(
                Box::new(Expr::Any),
                Box::new(Expr::Seq(
                    Box::new(Expr::Any),
                    Box::new(Expr::Named(
                        Box::new(Expr::Any),
                        proc_macro2::Ident::new("name", proc_macro2::Span::call_site())
                    ))
                )),
                
            )
        );
    }
}