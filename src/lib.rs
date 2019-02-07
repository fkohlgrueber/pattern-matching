
extern crate proc_macro;
use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use lazy_static::lazy_static;

mod parse;

use std::collections::HashMap;
use crate::parse::Expr as ParseExpr;
use crate::parse::Pattern;
use crate::parse::RepeatKind;

enum Ty {
    Alt,
    Seq,
    Opt
}

lazy_static!{
    static ref TYPES: HashMap<&'static str, Vec<Ty>> = {
        let mut p = HashMap::new();
        p.insert("Lit", vec!(Ty::Alt));
        p.insert("Array", vec!(Ty::Seq));
        p.insert("Test", vec!(Ty::Opt));
        p.insert("Char", vec!(Ty::Alt));
        p.insert("Bool", vec!(Ty::Alt));
        p.insert("Int", vec!(Ty::Alt));
        p
    };
}

#[proc_macro]
pub fn pattern(item: TokenStream) -> TokenStream {
    let parse_pattern = syn::parse_macro_input!(item as Pattern);
    let name = parse_pattern.name;
    let ty = parse_pattern.ty;
    let ty_str = ty.clone().into_token_stream().to_string();
    let root_ty = {
        if ty_str.starts_with("Alt") {
            Ty::Alt
        } else if ty_str.starts_with("Seq") {
            Ty::Seq
        } else if ty_str.starts_with("Opt") {
            Ty::Opt
        } else {
            panic!("Invalid Type provided!")
        }
    };
    let tokens = to_tokens(&parse_pattern.node, &root_ty);
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
    quote!(#ident ( #(#tokens),* ))
}

fn to_tokens_alt(parse_tree: &ParseExpr) -> proc_macro2::TokenStream {
    match parse_tree {
        ParseExpr::Any => quote!(Alt::Any),
        ParseExpr::Alt(a, b) => {
            let a_tokens = to_tokens_alt(a);
            let b_tokens = to_tokens_alt(b);
            quote!(Alt::Alt(Box::new(#a_tokens), Box::new(#b_tokens)))
        },
        ParseExpr::Node(ident, args) => {
            let tokens = node_to_tokens(ident, args);
            quote!(Alt::Elmt(#tokens))
        },
        ParseExpr::Lit(l) => {
            quote!(Alt::Elmt(#l))
        },
        ParseExpr::Named(e, i) => {
            let e_tokens = to_tokens_alt(e);
            quote!(Alt::Named(Box::new(#e_tokens), stringify!(#i).to_string()))
        },
        _ => panic!("Seq, Repeat and Empty aren't allowed when Alt<_> is expected")
    }
}

fn to_tokens_opt(parse_tree: &ParseExpr) -> proc_macro2::TokenStream {
    match parse_tree {
        ParseExpr::Any => quote!(Opt::Any),
        ParseExpr::Alt(a, b) => {
            let a_tokens = to_tokens_opt(a);
            let b_tokens = to_tokens_opt(b);
            quote!(Opt::Alt(Box::new(#a_tokens), Box::new(#b_tokens)))
        },
        ParseExpr::Node(ident, args) => {
            let tokens = node_to_tokens(ident, args);
            quote!(Opt::Elmt(#tokens))
        },
        ParseExpr::Lit(l) => {
            quote!(Opt::Elmt(#l))
        },
        ParseExpr::Named(e, i) => {
            let e_tokens = to_tokens_opt(e);
            quote!(Opt::Named(Box::new(#e_tokens), stringify!(#i).to_string()))
        },
        ParseExpr::Empty => quote!(Opt::None),
        ParseExpr::Repeat(e, RepeatKind::Optional) => {
            let e_tokens = to_tokens_opt(e);
            quote!(Opt::Alt(Box::new(#e_tokens), Box::new(Opt::None)))
        },
        ParseExpr::Repeat(_, _) => 
            panic!("`*`, `+` and `{..}` arent't allowed when Opt<_> is expected"),
        _ => panic!("Seq isn't allowed when Opt<_> is expected")
    }
}

fn to_tokens_seq(parse_tree: &ParseExpr) -> proc_macro2::TokenStream {
    match parse_tree {
        _ => panic!("Seq isn't implemented yet")
    }
}
