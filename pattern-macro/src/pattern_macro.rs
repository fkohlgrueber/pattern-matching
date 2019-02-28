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

#[derive(Debug, Clone, PartialEq)]
enum ResTy<T> {
    Elmt(T),
    Seq(Box<ResTy<T>>),
    Opt(Box<ResTy<T>>)
}

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

fn get_named_subpattern_types(input: &parse::Expr, ty: &PatTy) -> HashMap<Ident, ResTy<Ident>> {
    match input {
        parse::Expr::Node(id, args) => {
            let tys = TYPES.get(id.to_string().as_str()).expect("Unknown Node");
            if tys.len() != args.len() { panic!("Wrong number of arguments") }
            let hms = args.iter().zip(tys.iter()).map(
                |(e, (inner_ty, ty))| get_named_subpattern_types(e, &PatTy { inner_ty: Ident::new(inner_ty, proc_macro2::Span::call_site()), ty: ty.clone()})
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
                    let res_ty = match &i_ty {
                        // don't wrap with Option<_> if i_ty is Option<_> already
                        ResTy::Opt(_) => i_ty.clone(),
                        _ => ResTy::Opt(Box::new((*i_ty).clone()))
                    };
                    res.insert((*i).clone(), res_ty);
                }
            }
            for (i, i_ty) in &b_hm {
                if !a_hm.contains_key(&i) {
                    let res_ty = match &i_ty {
                        // don't wrap with Option<_> if i_ty is Option<_> already
                        ResTy::Opt(_) => i_ty.clone(),
                        _ => ResTy::Opt(Box::new((*i_ty).clone()))
                    };
                    res.insert((*i).clone(), res_ty);
                }
            }
            
            // elmts that are in both hashmaps
            for (i, i_ty) in &a_hm {
                if let Some(j_ty) = b_hm.get(&i) {
                    // exact equality
                    if i_ty == j_ty {
                        res.insert((*i).clone(), (*i_ty).clone());
                    // i_ty = Option<j_ty>
                    } else if i_ty == &ResTy::Opt(Box::new((*j_ty).clone())) {
                        res.insert((*i).clone(), (*i_ty).clone());
                    // Option<i_ty> = j_ty
                    } else if &ResTy::Opt(Box::new((*i_ty).clone())) == j_ty {
                        res.insert((*i).clone(), (*j_ty).clone());
                    } else {
                        panic!("Multiple occurances of the same #name need to have the same type.")
                    }
                }
            }

            res
        },
        parse::Expr::Named(e, id) => {
            let mut h = get_named_subpattern_types(e, ty);
            
            // inner type (e.g. a pattern_tree node) is provided by parameter (top-down)
            let inner_ty = &ty.inner_ty;

            // repeat type (single, optional, multiple) is determined by looking at own children (bottom-up)
            let repeat_type = get_repeat_type(e);

            let res_ty = match repeat_type {
                Ty::Alt => ResTy::Elmt(inner_ty.clone()),
                Ty::Seq => ResTy::Seq(Box::new(ResTy::Elmt(inner_ty.clone()))),
                Ty::Opt => ResTy::Opt(Box::new(ResTy::Elmt(inner_ty.clone()))),
            };
            
            h.insert(id.clone(), res_ty);
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

    // extract the type (Alt<_>, Seq<_>, Opt<_> or _)
    let pat_ty = PatTy {
        inner_ty: ty,
        ty: repeat_ty
    };

    // for each named subpattern, get its type
    let named_subpattern_types = get_named_subpattern_types(&node, &pat_ty);

    
    let result_tmp_items = named_subpattern_types.iter().map(
        |(k, v)| match v {
            ResTy::Elmt(e) => quote!( #k: Option<&'o A::#e>, ),
            ResTy::Opt(o) => match &**o {
                ResTy::Elmt(e) => quote!( #k: Option<&'o A::#e>, ),
                _ => panic!("This is not implemented yet!!")
            },
            ResTy::Seq(s) => match &**s {
                ResTy::Elmt(e) => quote!( #k: Vec<&'o A::#e>, ),
                _ => panic!("This is not implemented yet!!")
            },
        }
    ).collect::<Vec<_>>();


    let result_items = named_subpattern_types.iter().map(
        |(k, v)| match v {
            ResTy::Elmt(e) => quote!( #k: &'o A::#e, ),
            ResTy::Opt(o) => match &**o {
                ResTy::Elmt(e) => quote!( #k: Option<&'o A::#e>, ),
                _ => panic!("This is not implemented yet!!")
            },
            ResTy::Seq(s) => match &**s {
                ResTy::Elmt(e) => quote!( #k: Vec<&'o A::#e>, ),
                _ => panic!("This is not implemented yet!!")
            },
        }
    ).collect::<Vec<_>>();

    let init_tmp_items = named_subpattern_types.iter().map(
        |(k, v)| match v {
            ResTy::Elmt(_e) => quote!( #k: None, ),
            ResTy::Opt(o) => match &**o {
                ResTy::Elmt(_e) => quote!( #k: None, ),
                _ => panic!("This is not implemented yet!!")
            },
            ResTy::Seq(s) => match &**s {
                ResTy::Elmt(_e) => quote!( #k: vec!(), ),
                _ => panic!("This is not implemented yet!!")
            },
        }
    ).collect::<Vec<_>>();

    let init_items = named_subpattern_types.iter().map(
        |(k, v)| match v {
            ResTy::Elmt(_e) => quote!( #k: cx.#k.unwrap(), ),
            ResTy::Opt(o) => match &**o {
                ResTy::Elmt(_e) => quote!( #k: cx.#k, ),
                _ => panic!("This is not implemented yet!!")
            },
            ResTy::Seq(s) => match &**s {
                ResTy::Elmt(_e) => quote!( #k: cx.#k, ),
                _ => panic!("This is not implemented yet!!")
            },
        }
    ).collect::<Vec<_>>();


    let pattern_ty = match &pat_ty.ty {
        Ty::Alt => quote!( Alt ),
        Ty::Seq => quote!( Seq ),
        Ty::Opt => quote!( Opt ),
    };

    let tokens = to_tokens(&node, &pat_ty.ty, &named_subpattern_types);
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

            let pattern: pattern::pattern_match::matchers::#pattern_ty<
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

fn to_tokens(parse_tree: &ParseExpr, ty: &Ty, named_types: &HashMap<Ident, ResTy<Ident>>) -> proc_macro2::TokenStream {
    match ty {
        Ty::Alt => to_tokens_alt(parse_tree, named_types),
        Ty::Opt => to_tokens_opt(parse_tree, named_types),
        Ty::Seq => to_tokens_seq(parse_tree, named_types)
    }
}

fn node_to_tokens(ident: &proc_macro2::Ident, args: &Vec<ParseExpr>, named_types: &HashMap<Ident, ResTy<Ident>>) -> proc_macro2::TokenStream {
    let tys = TYPES.get(ident.to_string().as_str()).expect("Unknown Node");
    if tys.len() != args.len() { panic!("Wrong number of arguments") }
    let tokens = args.iter().zip(tys.iter()).map(
        |(e, (_inner_ty, ty))| to_tokens(e, ty, named_types)
    ).collect::<Vec<_>>();
    quote!(pattern::pattern_match::pattern_tree::variants:: #ident ( #(#tokens),* ))
}


fn to_tokens_alt(parse_tree: &ParseExpr, named_types: &HashMap<Ident, ResTy<Ident>>) -> proc_macro2::TokenStream {
    match parse_tree {
        ParseExpr::Any => quote!(pattern::pattern_match::matchers::Alt::Any),
        ParseExpr::Alt(a, b) => {
            let a_tokens = to_tokens_alt(a, named_types);
            let b_tokens = to_tokens_alt(b, named_types);
            quote!(pattern::pattern_match::matchers::Alt::Alt(Box::new(#a_tokens), Box::new(#b_tokens)))
        },
        ParseExpr::Node(ident, args) => {
            let tokens = node_to_tokens(ident, args, named_types);
            quote!(pattern::pattern_match::matchers::Alt::Elmt(Box::new(#tokens)))
        },
        ParseExpr::Lit(l) => {
            quote!(pattern::pattern_match::matchers::Alt::Elmt(Box::new(#l)))
        },
        ParseExpr::Named(e, i) => {
            let e_tokens = to_tokens_alt(e, named_types);
            quote!(
                pattern::pattern_match::matchers::Alt::Named(
                    Box::new(#e_tokens), 
                    |cx, elmt| {cx.#i = Some(elmt); cx}
                )
            )
        },
        _ => panic!("Seq, Repeat and Empty aren't allowed when Alt<_> is expected")
    }
}

fn to_tokens_opt(parse_tree: &ParseExpr, named_types: &HashMap<Ident, ResTy<Ident>>) -> proc_macro2::TokenStream {
    match parse_tree {
        ParseExpr::Any => quote!(pattern::pattern_match::matchers::Opt::Any),
        ParseExpr::Alt(a, b) => {
            let a_tokens = to_tokens_opt(a, named_types);
            let b_tokens = to_tokens_opt(b, named_types);
            quote!(pattern::pattern_match::matchers::Opt::Alt(Box::new(#a_tokens), Box::new(#b_tokens)))
        },
        ParseExpr::Node(ident, args) => {
            let tokens = node_to_tokens(ident, args, named_types);
            quote!(pattern::pattern_match::matchers::Opt::Elmt(Box::new(#tokens)))
        },
        ParseExpr::Lit(l) => {
            quote!(pattern::pattern_match::matchers::Opt::Elmt(Box::new(#l)))
        },
        ParseExpr::Named(e, i) => {
            let e_tokens = to_tokens_opt(e, named_types);
            quote!(
                pattern::pattern_match::matchers::Opt::Named(
                    Box::new(#e_tokens), 
                    |cx, elmt| {cx.#i = Some(elmt); cx}
                )
            )
        },
        ParseExpr::Empty => quote!(pattern::pattern_match::matchers::Opt::None),
        ParseExpr::Repeat(e, RepeatKind::Optional) => {
            let e_tokens = to_tokens_opt(e, named_types);
            quote!(pattern::pattern_match::matchers::Opt::Alt(Box::new(#e_tokens), Box::new(pattern::pattern_match::matchers::Opt::None)))
        },
        ParseExpr::Repeat(_, _) => 
            panic!("`*`, `+` and `{..}` arent't allowed when Opt<_> is expected"),
        _ => panic!("Seq isn't allowed when Opt<_> is expected")
    }
}

fn to_tokens_seq(parse_tree: &ParseExpr, named_types: &HashMap<Ident, ResTy<Ident>>) -> proc_macro2::TokenStream {
    match parse_tree {
        ParseExpr::Any => quote!(pattern::pattern_match::matchers::Seq::Any),
        ParseExpr::Alt(a, b) => {
            let a_tokens = to_tokens_seq(a, named_types);
            let b_tokens = to_tokens_seq(b, named_types);
            quote!(pattern::pattern_match::matchers::Seq::Alt(Box::new(#a_tokens), Box::new(#b_tokens)))
        },
        ParseExpr::Node(ident, args) => {
            let tokens = node_to_tokens(ident, args, named_types);
            quote!(pattern::pattern_match::matchers::Seq::Elmt(Box::new(#tokens)))
        },
        ParseExpr::Lit(l) => {
            quote!(pattern::pattern_match::matchers::Seq::Elmt(Box::new(#l)))
        },
        ParseExpr::Named(e, i) => {
            let ty = named_types.get(i).unwrap();
            let action = if let ResTy::Seq(_s) = ty {
                quote!( cx.#i.push(elmt); )
            } else {
                quote!( cx.#i = Some(elmt); )
            };
            let e_tokens = to_tokens_seq(e, named_types);
            quote!(
                pattern::pattern_match::matchers::Seq::Named(
                    Box::new(#e_tokens), 
                    |cx, elmt| {#action cx}
                )
            )
        },
        ParseExpr::Empty => quote!(pattern::pattern_match::matchers::Seq::Empty),
        ParseExpr::Repeat(e, r) => {
            let e_tokens = to_tokens_seq(e, named_types);
            let repeat_range = match r {
                RepeatKind::Any => quote!(pattern::pattern_match::matchers::RepeatRange { start: 0, end: None }),
                RepeatKind::Plus => quote!(pattern::pattern_match::matchers::RepeatRange { start: 1, end: None }),
                RepeatKind::Optional => quote!(pattern::pattern_match::matchers::RepeatRange { start: 0, end: Some(2) }),
                RepeatKind::Range(f, Some(t)) => quote!(pattern::pattern_match::matchers::RepeatRange { start: #f, end: Some((#t)+1) }),
                RepeatKind::Range(f, None) => quote!(pattern::pattern_match::matchers::RepeatRange { start: #f, end: None }),
                RepeatKind::Repeat(n) => quote!(pattern::pattern_match::matchers::RepeatRange { start: #n, end: Some((#n)+1) })
            };
            quote!(pattern::pattern_match::matchers::Seq::Repeat(Box::new(#e_tokens), #repeat_range))
        },
        ParseExpr::Seq(a, b) => {
            let a_tokens = to_tokens_seq(a, named_types);
            let b_tokens = to_tokens_seq(b, named_types);
            quote!(pattern::pattern_match::matchers::Seq::Seq(Box::new(#a_tokens), Box::new(#b_tokens)))
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