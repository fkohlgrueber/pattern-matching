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
pub struct Pattern {
    pub name: Ident,
    pub ty: syn::Ident,
    pub repeat_ty: common::Ty,
    pub node: ParseTree,
}

#[allow(clippy::module_name_repetitions)]
#[derive(PartialEq, Debug)]
pub enum ParseTree {
    Node(Ident, Vec<ParseTree>),
    Alt(Box<ParseTree>, Box<ParseTree>),
    Seq(Box<ParseTree>, Box<ParseTree>),
    Repeat(Box<ParseTree>, RepeatKind),
    Named(Box<ParseTree>, Ident),
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

impl fmt::Display for ParseTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseTree::Node(i, args) => write!(f, "{}({})", i, args.iter().map(|x| format!("{}", x).to_string()).collect::<Vec<_>>().join(", ")),
            ParseTree::Alt(left, right) => write!(f, "({} | {})", left, right),
            ParseTree::Seq(left, right) => write!(f, "({}; {})", left, right),
            ParseTree::Any => write!(f, "_"),
            ParseTree::Empty => write!(f, "()"),
            ParseTree::Repeat(e, r) => write!(f, "{}{}", e, match r {
                RepeatKind::Any => "*".to_string(),
                RepeatKind::Plus => "+".to_string(),
                RepeatKind::Optional => "?".to_string(),
                RepeatKind::Range(f, t) => format!("{{{}{}}}", 
                    f.value(),
                    t.as_ref().map_or("".to_string(), |t| format!(",{}", t.value()))
                ),
                RepeatKind::Repeat(r) => format!("{{{}}}", r.value())
            }),
            ParseTree::Named(e, i) => write!(f, "{}#{}", e, i),
            ParseTree::Lit(_e) => write!(f, "<expr>"),
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

impl Parse for Pattern {
    fn parse(input: ParseStream) -> Result<Self> {
        // parse name
        let name = input.parse()?;
        
        input.parse::<Token![:]>()?;
        
        // parse type (either `ident` or `ident<ident>`)
        let (ty, repeat_ty) = if input.peek2(Token![<]) {
            let repeat_ty = input.parse()?;
            input.parse::<Token![<]>()?;
            let ty = input.parse()?;
            input.parse::<Token![>]>()?;
            (ty, repeat_ty)
        } else {
            (input.parse()?, common::Ty::Alt)
        };
        
        input.parse::<Token![=]>()?;
        
        // parse pattern
        let node = input.parse()?;

        Ok(
            Self {
                name,
                ty,
                repeat_ty,
                node,
            }
        )
    }
}


impl Parse for ParseTree {
    fn parse(input: ParseStream) -> Result<Self> {
        ambiguous_expr(input)
    }
}

// Parse an arbitrary expression.
fn ambiguous_expr(input: ParseStream) -> Result<ParseTree> {
    let lhs = unary_expr(input)?;
    parse_expr(input, lhs, Precedence::Any)
}

fn unary_expr(input: ParseStream) -> Result<ParseTree> {
    trailer_expr(input)
}

fn trailer_expr(input: ParseStream) -> Result<ParseTree> {

    let mut e = atom_expr(input)?;
    
    // parse repeat syntax
    if input.peek(Token![*]) {
        input.parse::<Token![*]>()?;
        e = ParseTree::Repeat(Box::new(e), RepeatKind::Any);
    } else if input.peek(Token![+]) {
        input.parse::<Token![+]>()?;
        e = ParseTree::Repeat(Box::new(e), RepeatKind::Plus);
    } else if input.peek(Token![?]) {
        input.parse::<Token![?]>()?;
        e = ParseTree::Repeat(Box::new(e), RepeatKind::Optional);
    } else if input.peek(token::Brace) {
        let content;
        braced!(content in input);
        let f = content.parse::<syn::LitInt>()?;
        if content.is_empty() {
            e = ParseTree::Repeat(Box::new(e), RepeatKind::Repeat(f))
        } else {
            content.parse::<Token![,]>()?;
            let t = if content.is_empty() {
                None
            } else {
                Some(content.parse::<syn::LitInt>()?)
            };
            e = ParseTree::Repeat(Box::new(e), RepeatKind::Range(f, t));
        };
    }

    // parse named
    if input.peek(Token![#]) {
        input.parse::<Token![#]>()?;
        e = ParseTree::Named(Box::new(e), input.parse()?);
    }
    
    Ok(e)
}

fn atom_expr(input: ParseStream) -> Result<ParseTree> {
    if input.peek(Token![_]){
        input.parse::<Token![_]>()?;
        Ok(ParseTree::Any)
    } else if input.peek(Ident) && input.peek2(token::Paren) {
        let id = input.parse::<Ident>()?;
        let content;
        parenthesized!(content in input);
        let vals: Punctuated<ParseTree, Token![,]>;
        vals = content.parse_terminated(ParseTree::parse)?;
        Ok(ParseTree::Node(
            id,
            vals.into_iter().collect()
        ))
    } else if input.peek(Ident) {
        let id = input.parse::<Ident>()?;
        Ok(ParseTree::Node(
            id,
            vec!()
        ))
    } else if input.peek(token::Paren) {
        let content;
        parenthesized!(content in input);
        if content.is_empty() {
            return Ok(ParseTree::Empty);
        }
        content.parse()
    } else if input.peek(syn::Lit) {
        let e = input.parse()?;
        Ok(ParseTree::Lit(e))
    } else {
        Err(input.error("Expected _, (), <node>(...), <node> or a literal"))
    }
}

fn parse_expr(
    input: ParseStream,
    mut lhs: ParseTree,
    base: Precedence,
) -> Result<ParseTree> {
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
                BinOp::Seq => ParseTree::Seq(Box::new(lhs), Box::new(rhs)),
                BinOp::Or => ParseTree::Alt(Box::new(lhs), Box::new(rhs))
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

    fn parse_test(input: &'static str, exp: ParseTree) {
        let res = syn::parse_str::<ParseTree>(input);
        assert_eq!(res.ok(), Some(exp));
    }

    fn int_lit(input: u64) -> syn::LitInt {
        syn::LitInt::new(input, syn::IntSuffix::None, proc_macro2::Span::call_site())
    }

    #[test]
    fn any() {
        parse_test("_", ParseTree::Any);
    }

    #[test]
    fn empty() {
        parse_test(
            "()",
            ParseTree::Empty
        )
    }

    #[test]
    fn lit() {
        let exp = ParseTree::Lit(
            syn::Lit::Int(int_lit(123))
        );
        parse_test("123", exp);

        let exp = ParseTree::Lit(
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
            ParseTree::Named(
                Box::new(ParseTree::Any), 
                proc_macro2::Ident::new("test", proc_macro2::Span::call_site())
            )
        );
    }

    #[test]
    fn repeat() {
        parse_test(
            "_*", 
            ParseTree::Repeat(
                Box::new(ParseTree::Any), 
                RepeatKind::Any
            )
        );
        parse_test(
            "_+", 
            ParseTree::Repeat(
                Box::new(ParseTree::Any), 
                RepeatKind::Plus
            )
        );
        parse_test(
            "_?", 
            ParseTree::Repeat(
                Box::new(ParseTree::Any), 
                RepeatKind::Optional
            )
        );
        parse_test(
            "_{3, 4}", 
            ParseTree::Repeat(
                Box::new(ParseTree::Any), 
                RepeatKind::Range(
                    int_lit(3),
                    Some(int_lit(4)),
                )
            )
        );
        parse_test(
            "_{3, }", 
            ParseTree::Repeat(
                Box::new(ParseTree::Any), 
                RepeatKind::Range(
                    int_lit(3),
                    None,
                )
            )
        );
        parse_test(
            "_{3}", 
            ParseTree::Repeat(
                Box::new(ParseTree::Any), 
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
            ParseTree::Seq(
                Box::new(ParseTree::Any), 
                Box::new(ParseTree::Lit(syn::Lit::Int(int_lit(1))))
            )
        );
        parse_test(
            "1 2 3", 
            ParseTree::Seq(
                Box::new(ParseTree::Seq(
                    Box::new(ParseTree::Lit(syn::Lit::Int(int_lit(1)))),
                    Box::new(ParseTree::Lit(syn::Lit::Int(int_lit(2))))
                )),
                Box::new(ParseTree::Lit(syn::Lit::Int(int_lit(3))))
            )
        );
    }


    #[test]
    fn alt() {
        parse_test(
            "_ | 1", 
            ParseTree::Alt(
                Box::new(ParseTree::Any), 
                Box::new(ParseTree::Lit(syn::Lit::Int(int_lit(1))))
            )
        );
        parse_test(
            "1|2|3",
            ParseTree::Alt(
                Box::new(ParseTree::Alt(
                    Box::new(ParseTree::Lit(syn::Lit::Int(int_lit(1)))),
                    Box::new(ParseTree::Lit(syn::Lit::Int(int_lit(2))))
                )),
                Box::new(ParseTree::Lit(syn::Lit::Int(int_lit(3))))
            )
        );
    }

    #[test]
    fn node() {
        parse_test(
            "Test()", 
            ParseTree::Node(
                proc_macro2::Ident::new("Test", proc_macro2::Span::call_site()),
                vec!()
            )
        );
        parse_test(
            "Test(1)", 
            ParseTree::Node(
                proc_macro2::Ident::new("Test", proc_macro2::Span::call_site()),
                vec!(
                    ParseTree::Lit(syn::Lit::Int(int_lit(1)))
                )
            )
        );
        parse_test(
            "Test(1,2)", 
            ParseTree::Node(
                proc_macro2::Ident::new("Test", proc_macro2::Span::call_site()),
                vec!(
                    ParseTree::Lit(syn::Lit::Int(int_lit(1))),
                    ParseTree::Lit(syn::Lit::Int(int_lit(2)))
                )
            )
        );
    }

    #[test]
    fn precedence() {
        // alt < seq
        parse_test(
            "_ _ | _",  // ( _ _ ) | ( _ )
            ParseTree::Alt(
                Box::new(ParseTree::Seq(
                    Box::new(ParseTree::Any),
                    Box::new(ParseTree::Any)
                )),
                Box::new(ParseTree::Any)
            )
        );
        // alt < seq < named
        parse_test(
            "_ | _ _ #name",  // ( _ ) | ( _ ( _ #name ) )
            ParseTree::Alt(
                Box::new(ParseTree::Any),
                Box::new(ParseTree::Seq(
                    Box::new(ParseTree::Any),
                    Box::new(ParseTree::Named(
                        Box::new(ParseTree::Any),
                        proc_macro2::Ident::new("name", proc_macro2::Span::call_site())
                    ))
                )),
                
            )
        );
    }
}