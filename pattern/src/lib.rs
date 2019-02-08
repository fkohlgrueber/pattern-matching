
extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use quote::ToTokens;

mod parse;

use parse::Expr as ParseExpr;
use parse::RepeatKind;
use parse::Pattern;

use pattern_tree::Ty;
use pattern_tree::TYPES;

#[proc_macro]
pub fn pattern(item: TokenStream) -> TokenStream {
    let parse_pattern = syn::parse_macro_input!(item as Pattern);
    let name = parse_pattern.name;
    let ty = parse_pattern.ty;
    let ty_str = ty.clone().into_token_stream().to_string();
    // TODO: the type should be detected as part of the parsing step
    let root_ty = {
        if ty_str.starts_with("Alt") {
            Some(Ty::Alt)
        } else if ty_str.starts_with("Seq") {
            Some(Ty::Seq)
        } else if ty_str.starts_with("Opt") {
            Some(Ty::Opt)
        } else {
            None
        }
    };
    let tokens = match root_ty {
        Some(root_ty) => to_tokens(&parse_pattern.node, &root_ty),
        None => to_tokens_node(&parse_pattern.node)
    };
    quote!(
        lazy_static!{
            static ref #name: #ty = #tokens;
        }
    ).into()
}

fn to_tokens(parse_tree: &ParseExpr, ty: &Ty) -> proc_macro2::TokenStream {
    match ty {
        Ty::Alt => to_tokens_alt(parse_tree),
        Ty::Opt => to_tokens_opt(parse_tree),
        Ty::Seq => to_tokens_seq(parse_tree)
    }
}

fn node_to_tokens(ident: &proc_macro2::Ident, args: &Vec<ParseExpr>) -> proc_macro2::TokenStream {
    let tys = TYPES.get(ident.to_string().as_str()).expect("Unknown Node");
    if tys.len() != args.len() { panic!("Wrong number of arguments") }
    let tokens = args.iter().zip(tys.iter()).map(
        |(e, ty)| to_tokens(e, ty)
    ).collect::<Vec<_>>();
    quote!(pattern_tree::variants:: #ident ( #(#tokens),* ))
}

fn to_tokens_node(parse_tree: &ParseExpr) -> proc_macro2::TokenStream {
    match parse_tree {
        ParseExpr::Node(ident, args) => {
            let tokens = node_to_tokens(ident, args);
            quote!(#tokens)
        },
        _ => panic!("Expected node")
    }
}


fn to_tokens_alt(parse_tree: &ParseExpr) -> proc_macro2::TokenStream {
    match parse_tree {
        ParseExpr::Any => quote!(pattern_tree::matchers::Alt::Any),
        ParseExpr::Alt(a, b) => {
            let a_tokens = to_tokens_alt(a);
            let b_tokens = to_tokens_alt(b);
            quote!(pattern_tree::matchers::Alt::Alt(Box::new(#a_tokens), Box::new(#b_tokens)))
        },
        ParseExpr::Node(ident, args) => {
            let tokens = node_to_tokens(ident, args);
            quote!(pattern_tree::matchers::Alt::Elmt(#tokens))
        },
        ParseExpr::Lit(l) => {
            quote!(pattern_tree::matchers::Alt::Elmt(#l))
        },
        ParseExpr::Named(e, i) => {
            let e_tokens = to_tokens_alt(e);
            quote!(pattern_tree::matchers::Alt::Named(Box::new(#e_tokens), stringify!(#i).to_string()))
        },
        _ => panic!("Seq, Repeat and Empty aren't allowed when Alt<_> is expected")
    }
}

fn to_tokens_opt(parse_tree: &ParseExpr) -> proc_macro2::TokenStream {
    match parse_tree {
        ParseExpr::Any => quote!(pattern_tree::matchers::Opt::Any),
        ParseExpr::Alt(a, b) => {
            let a_tokens = to_tokens_opt(a);
            let b_tokens = to_tokens_opt(b);
            quote!(pattern_tree::matchers::Opt::Alt(Box::new(#a_tokens), Box::new(#b_tokens)))
        },
        ParseExpr::Node(ident, args) => {
            let tokens = node_to_tokens(ident, args);
            quote!(pattern_tree::matchers::Opt::Elmt(#tokens))
        },
        ParseExpr::Lit(l) => {
            quote!(pattern_tree::matchers::Opt::Elmt(#l))
        },
        ParseExpr::Named(e, i) => {
            let e_tokens = to_tokens_opt(e);
            quote!(pattern_tree::matchers::Opt::Named(Box::new(#e_tokens), stringify!(#i).to_string()))
        },
        ParseExpr::Empty => quote!(pattern_tree::matchers::Opt::None),
        ParseExpr::Repeat(e, RepeatKind::Optional) => {
            let e_tokens = to_tokens_opt(e);
            quote!(pattern_tree::matchers::Opt::Alt(Box::new(#e_tokens), Box::new(pattern_tree::matchers::Opt::None)))
        },
        ParseExpr::Repeat(_, _) => 
            panic!("`*`, `+` and `{..}` arent't allowed when Opt<_> is expected"),
        _ => panic!("Seq isn't allowed when Opt<_> is expected")
    }
}

fn to_tokens_seq(parse_tree: &ParseExpr) -> proc_macro2::TokenStream {
    match parse_tree {
        ParseExpr::Any => quote!(pattern_tree::matchers::Seq::Any),
        ParseExpr::Alt(a, b) => {
            let a_tokens = to_tokens_seq(a);
            let b_tokens = to_tokens_seq(b);
            quote!(pattern_tree::matchers::Seq::Alt(Box::new(#a_tokens), Box::new(#b_tokens)))
        },
        ParseExpr::Node(ident, args) => {
            let tokens = node_to_tokens(ident, args);
            quote!(pattern_tree::matchers::Seq::Elmt(Box::new(#tokens)))
        },
        ParseExpr::Lit(l) => {
            quote!(pattern_tree::matchers::Seq::Elmt(Box::new(#l)))
        },
        ParseExpr::Named(e, i) => {
            let e_tokens = to_tokens_seq(e);
            quote!(pattern_tree::matchers::Seq::Named(Box::new(#e_tokens), stringify!(#i).to_string()))
        },
        ParseExpr::Empty => quote!(pattern_tree::matchers::Seq::Empty),
        ParseExpr::Repeat(e, r) => {
            let e_tokens = to_tokens_seq(e);
            let repeat_range = match r {
                RepeatKind::Any => quote!(pattern_tree::matchers::RepeatRange { start: 0, end: None }),
                RepeatKind::Plus => quote!(pattern_tree::matchers::RepeatRange { start: 1, end: None }),
                RepeatKind::Optional => quote!(pattern_tree::matchers::RepeatRange { start: 0, end: Some(2) }),
                RepeatKind::Range(f, Some(t)) => quote!(pattern_tree::matchers::RepeatRange { start: #f, end: Some((#t)+1) }),
                RepeatKind::Range(f, None) => quote!(pattern_tree::matchers::RepeatRange { start: #f, end: None }),
                RepeatKind::Repeat(n) => quote!(pattern_tree::matchers::RepeatRange { start: #n, end: Some((#n)+1) })
            };
            quote!(pattern_tree::matchers::Seq::Repeat(Box::new(#e_tokens), #repeat_range))
        },
        ParseExpr::Seq(a, b) => {
            let a_tokens = to_tokens_seq(a);
            let b_tokens = to_tokens_seq(b);
            quote!(pattern_tree::matchers::Seq::Seq(Box::new(#a_tokens), Box::new(#b_tokens)))
        }
    }
}






#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
