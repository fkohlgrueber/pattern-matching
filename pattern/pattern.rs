#![recursion_limit="256"]

extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use quote::ToTokens;

mod parse;

use crate::parse::Expr as ParseExpr;
use crate::parse::RepeatKind;
use crate::parse::Pattern;

use pattern_tree::Ty;
use pattern_tree::TYPES;

#[proc_macro]
pub fn pattern(item: TokenStream) -> TokenStream {
    let parse_pattern = syn::parse_macro_input!(item as Pattern);
    let name = parse_pattern.name;
    let struct_name = proc_macro2::Ident::new(&(name.to_string() + "Struct"), proc_macro2::Span::call_site());
    let res_name = proc_macro2::Ident::new(&(name.to_string() + "_Res"), proc_macro2::Span::call_site());
    //let res_tmp_name = proc_macro2::Ident::new(&(name.to_string() + "ResTmp"), proc_macro2::Span::call_site());
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
        /*
        #[derive(Debug)]
        struct #res_name<'o, A>
        where A: MatchAssociations {
            // TODO: add inferred types here
            // this struct can be created from the tmp struct (see below)
            // the conversion happens by unwrapping all options. This can safely be done
            // because the pattern is known.
            var: &'o A::Lit,
            var2: &'o A::Bool,
            expr: &'o A::Expr,
        }

        #[derive(Default)]
        struct #res_tmp_name<'o, A>
        where A: MatchAssociations {
            // TODO: add inferred types here
            // in this struct, non-Vec types are wrapped in Option<...> so that the struct can 
            // be initialized by ::default()
            var: Option<&'o A::Lit>,
            var2: Option<&'o A::Bool>,
            expr: Option<&'o A::Expr>,
        }
        
        struct #name {}

        impl #name {
            /*
            fn is_match() {
                let pattern: #ty = #tokens;
                dbg!(pattern);
            }*/

            fn is_match_new<'o, A>(node: &'o A::Expr) -> Option< #res_name<'o, A> >
            where 
                A: 'o + MatchAssociations, 
                //for<'cx> pattern_tree::Expr<'cx, 'o, #res_tmp_name<'o, A>, A>: IsMatch<'cx, 'o, #res_tmp_name<'o, A>, A::Expr> 
            {
                let pattern = #tokens;

                //let mut res = #res_tmp_name::default();

                /*let (is_match, _res) = pattern.is_match(&mut res, node);
                if is_match {
                    Some(res)
                } else {
                    None
                }*/
                None
            }
        }
        */

        /*
        struct #struct_name {}

        static #name: #struct_name = #struct_name{};
        
        impl #struct_name {
            fn is_match<'o, A>(node: &'o A::Expr) -> bool
            where 
                A: 'o + pattern_tree::MatchAssociations, 
                for<'cx> pattern_tree::Expr<'cx, 'o, #res_name<'o, A>, A>: IsMatch<A::Expr> 
            {
                let pattern: #ty = #tokens;
                pattern.is_match(node)
            }
        }*/
        
        
        fn #name () {
            let pattern: #ty = #tokens;
            dbg!(pattern);
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
            quote!(pattern_tree::matchers::Alt::Elmt(Box::new(#tokens)))
        },
        ParseExpr::Lit(l) => {
            quote!(pattern_tree::matchers::Alt::Elmt(Box::new(#l)))
        },
        ParseExpr::Named(e, i) => {
            let e_tokens = to_tokens_alt(e);
            quote!(
                pattern_tree::matchers::Alt::Named(
                    Box::new(#e_tokens), 
                    |cx, elmt| {cx.#i = Some(elmt); cx}
                )
            )
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
            quote!(pattern_tree::matchers::Opt::Elmt(Box::new(#tokens)))
        },
        ParseExpr::Lit(l) => {
            quote!(pattern_tree::matchers::Opt::Elmt(Box::new(#l)))
        },
        ParseExpr::Named(e, i) => {
            let e_tokens = to_tokens_opt(e);
            quote!(
                pattern_tree::matchers::Opt::Named(
                    Box::new(#e_tokens), 
                    |cx, elmt| {cx.#i = Some(elmt); cx}
                )
            )
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
            quote!(
                pattern_tree::matchers::Seq::Named(
                    Box::new(#e_tokens), 
                    |cx, elmt| {cx.#i = Some(elmt); cx}
                )
            )
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
