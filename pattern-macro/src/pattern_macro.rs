#![recursion_limit="256"]
use std::collections::HashMap;
use syn::Ident;


extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;

mod parse;
mod result_struct;
mod named_subpattern_types;

use crate::named_subpattern_types::get_named_subpattern_types;
use crate::parse::ParseTree;
use crate::parse::RepeatKind;
use crate::parse::Pattern;
use crate::result_struct::gen_result_structs;

use common::Ty;
use pattern_match::pattern_tree::TYPES;

#[derive(Debug, Clone)]
struct PatTy {
    inner_ty: Ident,
    ty: Ty
}


#[proc_macro]
pub fn pattern(item: TokenStream) -> TokenStream {
    
    // parse the pattern
    let Pattern { name, ty, repeat_ty, node } = syn::parse_macro_input!(item as Pattern);

    // wrap parsed pattern with named `root` so that the pattern result struct has at least one item
    let node = ParseTree::Named(Box::new(node), proc_macro2::Ident::new("root", proc_macro2::Span::call_site()));
    
    // name of the result struct is <pattern_name>Struct, e.g. PatStruct
    let struct_name = proc_macro2::Ident::new(&(name.to_string() + "Struct"), proc_macro2::Span::call_site());
    
    // name of the temporary result struct is <pattern_name>TmpStruct, e.g. PatTmpStruct
    // this struct is used as the context during matching
    let struct_tmp_name = proc_macro2::Ident::new(&(name.to_string() + "TmpStruct"), proc_macro2::Span::call_site());

    // for each named subpattern, get its type
    let named_subpattern_types = get_named_subpattern_types(&node, &ty);


    // generate the actual pattern structure
    let tokens = to_tokens(&node, &repeat_ty, &named_subpattern_types);

    // generate result structs (and their impls)
    let result_structs = gen_result_structs(&struct_tmp_name, &struct_name, &named_subpattern_types);

    quote!(
        // result structs
        #result_structs
        
        // matching function
        fn #name <'o, A, P> (node: &'o P) -> Option<#struct_name<'o, A>> 
        where 
            A: pattern::pattern_match::pattern_tree::MatchAssociations<'o, Expr=P>,
            P: std::fmt::Debug,
            for<'cx> pattern::pattern_match::pattern_tree::Expr<'cx, 'o, #struct_tmp_name<'o, A>, A>: pattern::pattern_match::IsMatch<
                'cx, 
                'o, 
                #struct_tmp_name<'o, A>, 
                P
            >,
        {
            use pattern::pattern_match::IsMatch;

            // initialize the pattern
            let pattern: pattern::pattern_match::matchers::#repeat_ty<
                '_, 
                '_, 
                pattern::pattern_match::pattern_tree::Expr<
                    '_, 
                    '_, 
                    #struct_tmp_name<A>, 
                    A
                >, 
                #struct_tmp_name<A>, 
                A::Expr
            > = #tokens;

            // initialize the result struct
            let mut cx = #struct_tmp_name::new();

            // match input node against pattern
            let (r, cx_out) = pattern.is_match(&mut cx, node);
            
            if r {
                // convert cx to final struct and return
                Some(cx.into())
            } else {
                None
            }
        }
    ).into()
}

fn to_tokens(parse_tree: &ParseTree, ty: &Ty, named_types: &HashMap<Ident, PatTy>) -> proc_macro2::TokenStream {
    match ty {
        Ty::Alt => to_tokens_alt(parse_tree, named_types),
        Ty::Opt => to_tokens_opt(parse_tree, named_types),
        Ty::Seq => to_tokens_seq(parse_tree, named_types)
    }
}

fn node_to_tokens(ident: &proc_macro2::Ident, args: &Vec<ParseTree>, named_types: &HashMap<Ident, PatTy>) -> proc_macro2::TokenStream {
    let tys = TYPES.get(ident.to_string().as_str()).expect("Unknown Node");
    if tys.len() != args.len() { panic!("Wrong number of arguments") }
    let tokens = args.iter().zip(tys.iter()).map(
        |(e, (_inner_ty, ty))| to_tokens(e, ty, named_types)
    ).collect::<Vec<_>>();
    quote!(pattern::pattern_match::pattern_tree::variants:: #ident ( #(#tokens),* ))
}


fn to_tokens_alt(parse_tree: &ParseTree, named_types: &HashMap<Ident, PatTy>) -> proc_macro2::TokenStream {
    let matchers = quote!(pattern::pattern_match::matchers);
    match parse_tree {
        ParseTree::Any => quote!(#matchers::Alt::Any),
        ParseTree::Alt(a, b) => {
            let a_tokens = to_tokens_alt(a, named_types);
            let b_tokens = to_tokens_alt(b, named_types);
            quote!(#matchers::Alt::Alt(Box::new(#a_tokens), Box::new(#b_tokens)))
        },
        ParseTree::Node(ident, args) => {
            let tokens = node_to_tokens(ident, args, named_types);
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
            let e_tokens = to_tokens_alt(e, named_types);
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

fn to_tokens_opt(parse_tree: &ParseTree, named_types: &HashMap<Ident, PatTy>) -> proc_macro2::TokenStream {
    let matchers = quote!(pattern::pattern_match::matchers);
    match parse_tree {
        ParseTree::Any => quote!(#matchers::Opt::Any),
        ParseTree::Alt(a, b) => {
            let a_tokens = to_tokens_opt(a, named_types);
            let b_tokens = to_tokens_opt(b, named_types);
            quote!(#matchers::Opt::Alt(Box::new(#a_tokens), Box::new(#b_tokens)))
        },
        ParseTree::Node(ident, args) => {
            let tokens = node_to_tokens(ident, args, named_types);
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
            let e_tokens = to_tokens_opt(e, named_types);
            quote!(
                #matchers::Opt::Named(
                    Box::new(#e_tokens), 
                    |cx, elmt| {#action cx}
                )
            )
        },
        ParseTree::Empty => quote!(#matchers::Opt::None),
        ParseTree::Repeat(e, RepeatKind::Optional) => {
            let e_tokens = to_tokens_opt(e, named_types);
            quote!(#matchers::Opt::Alt(Box::new(#e_tokens), Box::new(#matchers::Opt::None)))
        },
        ParseTree::Repeat(_, _) => 
            panic!("`*`, `+` and `{..}` arent't allowed when Opt<_> is expected"),
        _ => panic!("Seq isn't allowed when Opt<_> is expected")
    }
}

fn to_tokens_seq(parse_tree: &ParseTree, named_types: &HashMap<Ident, PatTy>) -> proc_macro2::TokenStream {
    let matchers = quote!(pattern::pattern_match::matchers);
    match parse_tree {
        ParseTree::Any => quote!(#matchers::Seq::Any),
        ParseTree::Alt(a, b) => {
            let a_tokens = to_tokens_seq(a, named_types);
            let b_tokens = to_tokens_seq(b, named_types);
            quote!(#matchers::Seq::Alt(Box::new(#a_tokens), Box::new(#b_tokens)))
        },
        ParseTree::Node(ident, args) => {
            let tokens = node_to_tokens(ident, args, named_types);
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
            let e_tokens = to_tokens_seq(e, named_types);
            quote!(
                #matchers::Seq::Named(
                    Box::new(#e_tokens), 
                    |cx, elmt| {#action cx}
                )
            )
        },
        ParseTree::Empty => quote!(#matchers::Seq::Empty),
        ParseTree::Repeat(e, r) => {
            let e_tokens = to_tokens_seq(e, named_types);
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
            let a_tokens = to_tokens_seq(a, named_types);
            let b_tokens = to_tokens_seq(b, named_types);
            quote!(#matchers::Seq::Seq(Box::new(#a_tokens), Box::new(#b_tokens)))
        }
    }
}





/*
#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
*/