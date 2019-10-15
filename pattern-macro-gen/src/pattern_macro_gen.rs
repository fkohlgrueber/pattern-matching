//! This crate provides the `gen_pattern_macro!()` macro which can be used to
//! generate pattern macros. See the macro's documentation for details.
#![recursion_limit="512"]
#![feature(rustc_private)]
use std::collections::HashMap;
use syn::Ident;

extern crate rustc_data_structures;
extern crate proc_macro;

mod result_struct;
mod named_subpattern_types;

pub use quote::quote;
pub use syn;
pub use proc_macro2;

pub use crate::named_subpattern_types::get_named_subpattern_types;
pub use pattern_parse::{ParseTree, RepeatKind, Pattern};

use common::Ty;

pub use crate::result_struct::gen_result_structs;
pub use pattern_macro_gen_macro::gen_pattern_macro;


#[derive(Debug, Clone)]
pub struct PatTy {
    inner_ty: Ident,
    ty: Ty
}


/// Checks whether the pattern `input` contains pattern-func calls (lowercase function calls)
pub fn needs_expansion(input: &ParseTree) -> Option<String> {
    match input {
        ParseTree::Node(id, args) => {
            let s = id.to_string();
            if s.chars().next().unwrap().is_lowercase() {
                Some(s)
            } else {
                for arg in args {
                    let n = needs_expansion(arg);
                    if n.is_some() {
                        return n;
                    }
                }
                None
            }
        },
        ParseTree::Alt(a, b) => needs_expansion(a).or_else(|| needs_expansion(b)),
        ParseTree::Named(e, _id) => needs_expansion(e),
        ParseTree::Lit(_) |
        ParseTree::Any |
        ParseTree::Empty => None,
        ParseTree::Seq(a, b) => needs_expansion(a).or_else(|| needs_expansion(b)),
        ParseTree::Repeat(e, _r) => needs_expansion(e),
    }
}

pub fn to_tokens(
    parse_tree: &ParseTree, 
    ty: Ty, 
    named_types: &HashMap<Ident, PatTy>,
    types: &rustc_data_structures::fx::FxHashMap<&'static str, Vec<(&'static str, common::Ty)>>,
    module: &proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    match ty {
        Ty::Alt => to_tokens_alt(parse_tree, named_types, types, module),
        Ty::Opt => to_tokens_opt(parse_tree, named_types, types, module),
        Ty::Seq => to_tokens_seq(parse_tree, named_types, types, module)
    }
}

fn node_to_tokens(
    ident: &proc_macro2::Ident, 
    args: &[ParseTree], 
    named_types: &HashMap<Ident, PatTy>,
    types: &rustc_data_structures::fx::FxHashMap<&'static str, Vec<(&'static str, common::Ty)>>,
    module: &proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let tys = types.get(ident.to_string().as_str()).expect("Unknown Node");
    if tys.len() != args.len() { panic!("Wrong number of arguments") }
    let tokens = args.iter().zip(tys.iter()).map(
        |(e, (_inner_ty, ty))| to_tokens(e, *ty, named_types, types, module)
    ).collect::<Vec<_>>();
    let args = if args.is_empty() {
        quote!( )
    } else {
        quote!( ( #(#tokens),* ) )
    };
    quote!(pattern::#module::variants:: #ident #args)
}


fn to_tokens_alt(
    parse_tree: &ParseTree, 
    named_types: &HashMap<Ident, PatTy>,
    types: &rustc_data_structures::fx::FxHashMap<&'static str, Vec<(&'static str, common::Ty)>>,
    module: &proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let matchers = quote!(pattern::matchers);
    match parse_tree {
        ParseTree::Any => quote!(#matchers::Alt::Any),
        ParseTree::Alt(a, b) => {
            let a_tokens = to_tokens_alt(a, named_types, types, module);
            let b_tokens = to_tokens_alt(b, named_types, types, module);
            quote!(#matchers::Alt::Alt(Box::new(#a_tokens), Box::new(#b_tokens)))
        },
        ParseTree::Node(ident, args) => {
            let tokens = node_to_tokens(ident, args, named_types, types, module);
            quote!(#matchers::Alt::Elmt(Box::new(#tokens)))
        },
        ParseTree::Lit(l) => {
            quote!(#matchers::Alt::Elmt(Box::new(#l)))
        },
        ParseTree::Named(e, i) => {
            let ty = named_types.get(i).unwrap();
            let action = if let Ty::Seq = &ty.ty {
                quote!( cx.#i.push(elmt); )
            } else {
                quote!( cx.#i = Some(elmt); )
            };
            let e_tokens = to_tokens_alt(e, named_types, types, module);
            quote!(
                #matchers::Alt::Named(
                    Box::new(#e_tokens), 
                    |cx, elmt| {#action cx}
                )
            )
        },
        _ => panic!("Seq, Repeat and Empty aren't allowed when Alt<_> is expected")
    }
}

#[allow(clippy::panic_params)]
fn to_tokens_opt(
    parse_tree: &ParseTree, 
    named_types: &HashMap<Ident, PatTy>,    
    types: &rustc_data_structures::fx::FxHashMap<&'static str, Vec<(&'static str, common::Ty)>>,
    module: &proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let matchers = quote!(pattern::matchers);
    match parse_tree {
        ParseTree::Any => quote!(#matchers::Opt::Any),
        ParseTree::Alt(a, b) => {
            let a_tokens = to_tokens_opt(a, named_types, types, module);
            let b_tokens = to_tokens_opt(b, named_types, types, module);
            quote!(#matchers::Opt::Alt(Box::new(#a_tokens), Box::new(#b_tokens)))
        },
        ParseTree::Node(ident, args) => {
            let tokens = node_to_tokens(ident, args, named_types, types, module);
            quote!(#matchers::Opt::Elmt(Box::new(#tokens)))
        },
        ParseTree::Lit(l) => {
            quote!(#matchers::Opt::Elmt(Box::new(#l)))
        },
        ParseTree::Named(e, i) => {
            let ty = named_types.get(i).unwrap();
            let action = if let Ty::Seq = &ty.ty {
                quote!( cx.#i.push(elmt); )
            } else {
                quote!( cx.#i = Some(elmt); )
            };
            let e_tokens = to_tokens_opt(e, named_types, types, module);
            quote!(
                #matchers::Opt::Named(
                    Box::new(#e_tokens), 
                    |cx, elmt| {#action cx}
                )
            )
        },
        ParseTree::Empty => quote!(#matchers::Opt::None),
        ParseTree::Repeat(e, RepeatKind::Optional) => {
            let e_tokens = to_tokens_opt(e, named_types, types, module);
            quote!(#matchers::Opt::Alt(Box::new(#e_tokens), Box::new(#matchers::Opt::None)))
        },
        ParseTree::Repeat(_, _) => 
            panic!("`*`, `+` and `{..}` arent't allowed when Opt<_> is expected"),
        _ => panic!("Seq isn't allowed when Opt<_> is expected")
    }
}

fn to_tokens_seq(
    parse_tree: &ParseTree, 
    named_types: &HashMap<Ident, PatTy>,    
    types: &rustc_data_structures::fx::FxHashMap<&'static str, Vec<(&'static str, common::Ty)>>,
    module: &proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let matchers = quote!(pattern::matchers);
    match parse_tree {
        ParseTree::Any => quote!(#matchers::Seq::Any),
        ParseTree::Alt(a, b) => {
            let a_tokens = to_tokens_seq(a, named_types, types, module);
            let b_tokens = to_tokens_seq(b, named_types, types, module);
            quote!(#matchers::Seq::Alt(Box::new(#a_tokens), Box::new(#b_tokens)))
        },
        ParseTree::Node(ident, args) => {
            let tokens = node_to_tokens(ident, args, named_types, types, module);
            quote!(#matchers::Seq::Elmt(Box::new(#tokens)))
        },
        ParseTree::Lit(l) => {
            quote!(#matchers::Seq::Elmt(Box::new(#l)))
        },
        ParseTree::Named(e, i) => {
            let ty = named_types.get(i).unwrap();
            let action = if let Ty::Seq = &ty.ty {
                quote!( cx.#i.push(elmt); )
            } else {
                quote!( cx.#i = Some(elmt); )
            };
            let e_tokens = to_tokens_seq(e, named_types, types, module);
            quote!(
                #matchers::Seq::Named(
                    Box::new(#e_tokens), 
                    |cx, elmt| {#action cx}
                )
            )
        },
        ParseTree::Empty => quote!(#matchers::Seq::Empty),
        ParseTree::Repeat(e, r) => {
            let e_tokens = to_tokens_seq(e, named_types, types, module);
            let (start, end) = match r {
                RepeatKind::Any => (quote!(0), quote!(None)),
                RepeatKind::Plus => (quote!(1), quote!(None)),
                RepeatKind::Optional => (quote!(0), quote!(Some(2))),
                RepeatKind::Range(f, Some(t)) => (quote!(#f), quote!(Some((#t)+1))),
                RepeatKind::Range(f, None) => (quote!(#f), quote!(None)),
                RepeatKind::Repeat(n) => (quote!(#n), quote!(Some((#n)+1))),
            };
            quote!(
                #matchers::Seq::Repeat(
                    Box::new(#e_tokens), 
                    #matchers::RepeatRange { start: #start, end: #end }
                )
            )
        },
        ParseTree::Seq(a, b) => {
            let a_tokens = to_tokens_seq(a, named_types, types, module);
            let b_tokens = to_tokens_seq(b, named_types, types, module);
            quote!(#matchers::Seq::Seq(Box::new(#a_tokens), Box::new(#b_tokens)))
        }
    }
}
