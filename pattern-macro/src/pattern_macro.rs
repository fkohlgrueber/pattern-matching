#![recursion_limit="256"]
use std::collections::HashMap;
use syn::Ident;


extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;

mod parse;

use crate::parse::Expr as ParseExpr;
use crate::parse::RepeatKind;
use crate::parse::Pattern;

use common::Ty;
use pattern_match::pattern_tree::TYPES;

#[derive(Debug, Clone)]
struct PatTy {
    inner_ty: Ident,
    ty: Ty
}

fn get_repeat_type(input: &parse::Expr) -> Ty {
    match input {
        parse::Expr::Any => Ty::Alt,
        parse::Expr::Empty => Ty::Seq,
        parse::Expr::Lit(_) => Ty::Alt,
        parse::Expr::Node(_, _) => Ty::Alt,
        parse::Expr::Alt(a, b) => {
            match (get_repeat_type(a), get_repeat_type(b)) {
                (Ty::Seq, _) | (_, Ty::Seq) => Ty::Seq,
                (Ty::Opt, _) | (_, Ty::Opt) => Ty::Opt,
                (Ty::Alt, Ty::Alt) => Ty::Alt
            }
        },
        parse::Expr::Seq(_, _) => Ty::Seq,
        parse::Expr::Repeat(e, r) => {
            let c = get_repeat_type(e);
            match (c, r) {
                (Ty::Alt, RepeatKind::Optional) => Ty::Opt,
                (Ty::Opt, RepeatKind::Optional) => Ty::Opt,
                _ => Ty::Seq
            }
        },
        parse::Expr::Named(e, _) => get_repeat_type(e)
    }
}

fn get_named_subpattern_types(input: &parse::Expr, ty: &Ident) -> HashMap<Ident, PatTy> {
    match input {
        parse::Expr::Node(id, args) => {
            let tys = TYPES.get(id.to_string().as_str()).expect("Unknown Node");
            if tys.len() != args.len() { panic!("Wrong number of arguments") }
            let hms = args.iter().zip(tys.iter()).map(
                |(e, (inner_ty, _ty))| get_named_subpattern_types(e, &Ident::new(inner_ty, proc_macro2::Span::call_site()))
            ).collect::<Vec<_>>();
            
            let mut res = HashMap::new();

            for hm in hms {
                for (k, v) in hm {
                    if res.contains_key(&k) {
                        panic!("Multiple occurrences of the same variable aren't allowed!")
                    }
                    res.insert(k, v);
                }
            }

            res
        },
        parse::Expr::Alt(a, b) => {
            let a_hm = get_named_subpattern_types(a, ty);
            let b_hm = get_named_subpattern_types(b, ty);
            let mut res = HashMap::new();

            // add unique elements
            // if an element is only present on one branch, it's type needs to be Option<_>
            for (i, i_ty) in &a_hm {
                if !b_hm.contains_key(&i) {
                    let res_ty = PatTy {
                        inner_ty: i_ty.inner_ty.clone(),
                        ty: match &i_ty.ty {
                            Ty::Alt => Ty::Opt,
                            t => t.clone()
                        }
                    };
                    res.insert((*i).clone(), res_ty);
                }
            }
            for (i, i_ty) in &b_hm {
                if !a_hm.contains_key(&i) {
                    let res_ty = PatTy {
                        inner_ty: i_ty.inner_ty.clone(),
                        ty: match &i_ty.ty {
                            Ty::Alt => Ty::Opt,
                            t => t.clone()
                        }
                    };
                    res.insert((*i).clone(), res_ty);
                }
            }
            
            // elmts that are in both hashmaps
            for (i, i_ty) in &a_hm {
                if let Some(j_ty) = b_hm.get(&i) {
                    if i_ty.inner_ty != j_ty.inner_ty {
                        panic!("Multiple occurances of the same #name need to have the same type.")
                    }
                    
                    let res_ty = PatTy {
                        inner_ty: i_ty.inner_ty.clone(),
                        ty: match (&i_ty.ty, &j_ty.ty) {
                            // if one is Seq => Seq
                            // if one is Opt => Opt
                            // else Alt
                            (Ty::Alt, Ty::Alt) => Ty::Alt,
                            (Ty::Seq, _) => Ty::Seq,
                            (_, Ty::Seq) => Ty::Seq,
                            _ => Ty::Opt
                        }
                    };
                    res.insert((*i).clone(), res_ty);
                }
            }

            res
        },
        parse::Expr::Named(e, id) => {
            let mut h = get_named_subpattern_types(e, ty);
            
            // inner type (e.g. a pattern_tree node) is provided by parameter (top-down)
            let inner_ty = ty;

            // repeat type (single, optional, multiple) is determined by looking at own children (bottom-up)
            let repeat_type = get_repeat_type(e);
            
            h.insert(id.clone(), PatTy { ty: repeat_type, inner_ty: inner_ty.clone()});
            h
        },
        parse::Expr::Lit(_l) => HashMap::new(),
        parse::Expr::Any => HashMap::new(),
        
        parse::Expr::Empty => HashMap::new(),
        parse::Expr::Seq(a, b) => {
            let a_hm = get_named_subpattern_types(a, ty);
            let b_hm = get_named_subpattern_types(b, ty);
            let mut res = HashMap::new();

            for (k, v) in a_hm {
                if res.contains_key(&k) {
                    panic!("Multiple occurrences of the same variable aren't allowed!")
                }
                res.insert(k, v);
            }
            for (k, v) in b_hm {
                if res.contains_key(&k) {
                    panic!("Multiple occurrences of the same variable aren't allowed!")
                }
                res.insert(k, v);
            }

            res
        },
        parse::Expr::Repeat(e, _r) => get_named_subpattern_types(e, ty),
    }
}


#[proc_macro]
pub fn pattern(item: TokenStream) -> TokenStream {
    
    // parse the pattern
    let Pattern { name, ty, repeat_ty, node } = syn::parse_macro_input!(item as Pattern);

    // wrap parsed pattern with named `root` so that the pattern result struct has at least one item
    let node = parse::Expr::Named(Box::new(node), proc_macro2::Ident::new("root", proc_macro2::Span::call_site()));
    
    // name of the result struct is <pattern_name>Struct, e.g. PatStruct
    let struct_name = proc_macro2::Ident::new(&(name.to_string() + "Struct"), proc_macro2::Span::call_site());
    
    // name of the temporary result struct is <pattern_name>TmpStruct, e.g. PatTmpStruct
    // this struct is used as the context during matching
    let struct_tmp_name = proc_macro2::Ident::new(&(name.to_string() + "TmpStruct"), proc_macro2::Span::call_site());

    // for each named subpattern, get its type
    let named_subpattern_types = get_named_subpattern_types(&node, &ty);

    
    let result_tmp_items = named_subpattern_types.iter().map(
        |(k, v)| {
            let e = &v.inner_ty; 
            match &v.ty {
                Ty::Alt => quote!( #k: Option<&'o A::#e>, ),
                Ty::Opt => quote!( #k: Option<&'o A::#e>, ),
                Ty::Seq => quote!( #k: Vec<&'o A::#e>, ),
            }
        }
    ).collect::<Vec<_>>();


    let result_items = named_subpattern_types.iter().map(
        |(k, v)| {
            let e = &v.inner_ty; 
            match &v.ty {
                Ty::Alt => quote!( #k: &'o A::#e, ),
                Ty::Opt => quote!( #k: Option<&'o A::#e>, ),
                Ty::Seq => quote!( #k: Vec<&'o A::#e>, ),
            }
        }
    ).collect::<Vec<_>>();

    let init_tmp_items = named_subpattern_types.iter().map(
        |(k, v)| {
            match &v.ty {
                Ty::Alt => quote!( #k: None, ),
                Ty::Opt => quote!( #k: None, ),
                Ty::Seq => quote!( #k: vec!(), ),
            }
        }
    ).collect::<Vec<_>>();

    let init_items = named_subpattern_types.iter().map(
        |(k, v)| {
            match &v.ty {
                Ty::Alt => quote!( #k: cx.#k.unwrap(), ),
                Ty::Opt => quote!( #k: cx.#k, ),
                Ty::Seq => quote!( #k: cx.#k, ),
            }
        }
    ).collect::<Vec<_>>();

    let tokens = to_tokens(&node, &repeat_ty, &named_subpattern_types);
    quote!(

        #[derive(Debug)]
        pub struct #struct_name<'o, A>
        where A: pattern::pattern_match::pattern_tree::MatchAssociations<'o> {
            #(#result_items)*
        }

        #[derive(Debug)]
        pub struct #struct_tmp_name<'o, A>
        where A: pattern::pattern_match::pattern_tree::MatchAssociations<'o> {
            #(#result_tmp_items)*
        }
        
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

            let mut cx = #struct_tmp_name {
                #(#init_tmp_items)*
            };

            let (r, cx_out) = pattern.is_match(&mut cx, node);
            if r {
                Some(#struct_name {
                    #(#init_items)*
                })
            } else {
                None
            }
        }
    ).into()
}

fn to_tokens(parse_tree: &ParseExpr, ty: &Ty, named_types: &HashMap<Ident, PatTy>) -> proc_macro2::TokenStream {
    match ty {
        Ty::Alt => to_tokens_alt(parse_tree, named_types),
        Ty::Opt => to_tokens_opt(parse_tree, named_types),
        Ty::Seq => to_tokens_seq(parse_tree, named_types)
    }
}

fn node_to_tokens(ident: &proc_macro2::Ident, args: &Vec<ParseExpr>, named_types: &HashMap<Ident, PatTy>) -> proc_macro2::TokenStream {
    let tys = TYPES.get(ident.to_string().as_str()).expect("Unknown Node");
    if tys.len() != args.len() { panic!("Wrong number of arguments") }
    let tokens = args.iter().zip(tys.iter()).map(
        |(e, (_inner_ty, ty))| to_tokens(e, ty, named_types)
    ).collect::<Vec<_>>();
    quote!(pattern::pattern_match::pattern_tree::variants:: #ident ( #(#tokens),* ))
}


fn to_tokens_alt(parse_tree: &ParseExpr, named_types: &HashMap<Ident, PatTy>) -> proc_macro2::TokenStream {
    let matchers = quote!(pattern::pattern_match::matchers);
    match parse_tree {
        ParseExpr::Any => quote!(#matchers::Alt::Any),
        ParseExpr::Alt(a, b) => {
            let a_tokens = to_tokens_alt(a, named_types);
            let b_tokens = to_tokens_alt(b, named_types);
            quote!(#matchers::Alt::Alt(Box::new(#a_tokens), Box::new(#b_tokens)))
        },
        ParseExpr::Node(ident, args) => {
            let tokens = node_to_tokens(ident, args, named_types);
            quote!(#matchers::Alt::Elmt(Box::new(#tokens)))
        },
        ParseExpr::Lit(l) => {
            quote!(#matchers::Alt::Elmt(Box::new(#l)))
        },
        ParseExpr::Named(e, i) => {
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

fn to_tokens_opt(parse_tree: &ParseExpr, named_types: &HashMap<Ident, PatTy>) -> proc_macro2::TokenStream {
    let matchers = quote!(pattern::pattern_match::matchers);
    match parse_tree {
        ParseExpr::Any => quote!(#matchers::Opt::Any),
        ParseExpr::Alt(a, b) => {
            let a_tokens = to_tokens_opt(a, named_types);
            let b_tokens = to_tokens_opt(b, named_types);
            quote!(#matchers::Opt::Alt(Box::new(#a_tokens), Box::new(#b_tokens)))
        },
        ParseExpr::Node(ident, args) => {
            let tokens = node_to_tokens(ident, args, named_types);
            quote!(#matchers::Opt::Elmt(Box::new(#tokens)))
        },
        ParseExpr::Lit(l) => {
            quote!(#matchers::Opt::Elmt(Box::new(#l)))
        },
        ParseExpr::Named(e, i) => {
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
        ParseExpr::Empty => quote!(#matchers::Opt::None),
        ParseExpr::Repeat(e, RepeatKind::Optional) => {
            let e_tokens = to_tokens_opt(e, named_types);
            quote!(#matchers::Opt::Alt(Box::new(#e_tokens), Box::new(#matchers::Opt::None)))
        },
        ParseExpr::Repeat(_, _) => 
            panic!("`*`, `+` and `{..}` arent't allowed when Opt<_> is expected"),
        _ => panic!("Seq isn't allowed when Opt<_> is expected")
    }
}

fn to_tokens_seq(parse_tree: &ParseExpr, named_types: &HashMap<Ident, PatTy>) -> proc_macro2::TokenStream {
    let matchers = quote!(pattern::pattern_match::matchers);
    match parse_tree {
        ParseExpr::Any => quote!(#matchers::Seq::Any),
        ParseExpr::Alt(a, b) => {
            let a_tokens = to_tokens_seq(a, named_types);
            let b_tokens = to_tokens_seq(b, named_types);
            quote!(#matchers::Seq::Alt(Box::new(#a_tokens), Box::new(#b_tokens)))
        },
        ParseExpr::Node(ident, args) => {
            let tokens = node_to_tokens(ident, args, named_types);
            quote!(#matchers::Seq::Elmt(Box::new(#tokens)))
        },
        ParseExpr::Lit(l) => {
            quote!(#matchers::Seq::Elmt(Box::new(#l)))
        },
        ParseExpr::Named(e, i) => {
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
        ParseExpr::Empty => quote!(#matchers::Seq::Empty),
        ParseExpr::Repeat(e, r) => {
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
        ParseExpr::Seq(a, b) => {
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