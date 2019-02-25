#![recursion_limit="256"]
use std::collections::HashMap;
use syn::Ident;
use std::fmt::Display;


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

#[derive(Debug, Clone, PartialEq)]
enum ResTy<T> {
    Elmt(T),
    Seq(Box<ResTy<T>>),
    Opt(Box<ResTy<T>>)
}

impl Display for ResTy<Ident> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            ResTy::Elmt(i) => write!(f, "{}", i.to_string()),
            ResTy::Seq(e) => write!(f, "Vec<{}>", e),
            ResTy::Opt(e) => write!(f, "Option<{}>", e),
        }
    }
}

#[derive(Debug, Clone)]
struct PatTy {
    inner_ty: Ident,
    ty: PatTy_
}

#[derive(Debug, Clone)]
enum PatTy_ {
    Alt,
    Seq,
    Opt
}

impl From<&Ty> for PatTy_ {
    fn from(other: &Ty) -> PatTy_ {
        match other {
            Ty::Alt => PatTy_::Alt,
            Ty::Seq => PatTy_::Seq,
            Ty::Opt => PatTy_::Opt,
        }
    }
}

fn to_res_ty(input: &PatTy) -> ResTy<Ident> {
    match &input.ty {
        PatTy_::Alt => ResTy::Elmt(input.inner_ty.clone()),
        PatTy_::Seq => ResTy::Seq(Box::new(ResTy::Elmt(input.inner_ty.clone()))),
        PatTy_::Opt => ResTy::Opt(Box::new(ResTy::Elmt(input.inner_ty.clone()))),
    }
}

fn get_named_subpattern_types(input: &parse::Expr, ty: &PatTy) -> HashMap<Ident, ResTy<Ident>> {
    match input {
        parse::Expr::Node(id, args) => {
            // TODO: continue here!
            
            let tys = TYPES.get(id.to_string().as_str()).expect("Unknown Node");
            if tys.len() != args.len() { panic!("Wrong number of arguments") }
            let hms = args.iter().zip(tys.iter()).map(
                |(e, (inner_ty, ty))| get_named_subpattern_types(e, &PatTy { inner_ty: Ident::new(inner_ty, proc_macro2::Span::call_site()), ty: ty.into()})
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
            for (i, i_ty) in &a_hm {
                if !b_hm.contains_key(&i) {
                    res.insert((*i).clone(), ResTy::Opt(Box::new((*i_ty).clone())));
                }
            }
            for (i, i_ty) in &b_hm {
                if !a_hm.contains_key(&i) {
                    res.insert((*i).clone(), ResTy::Opt(Box::new((*i_ty).clone())));
                }
            }

            // elmts that are in both hashmaps
            for (i, i_ty) in &a_hm {
                if let Some(j_ty) = b_hm.get(&i) {
                    if i_ty == j_ty {
                        res.insert((*i).clone(), (*i_ty).clone());
                    } else {
                        panic!("Multiple occurances of the same #name need to have the same type.")
                    }
                }
            }

            res
        },
        parse::Expr::Named(e, id) => {
            let mut h = get_named_subpattern_types(e, ty);
            h.insert(id.clone(), to_res_ty(ty));
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

fn print_hm(input: &HashMap<Ident, ResTy<Ident>>) {
    println!("struct Result {{");
    for (k, v) in input {
        println!("    {}: {}", k.to_string(), v)
    }
    println!("}}\n")
}

fn get_simple_path(ty: &syn::Type) -> Option<&Ident> {
    if let syn::Type::Path(p) = &ty {
        if let Some(ps) = p.path.segments.last() {
            Some(&ps.value().ident)
        } else {
            None
        }
    } else {
        None
    }
}

fn get_inner_ty(abga: &syn::AngleBracketedGenericArguments) -> Option<&Ident> {
    for ga in &abga.args {
        if let syn::GenericArgument::Lifetime(_) = ga {
            continue;
        } else if let syn::GenericArgument::Type(t) = ga {
            return get_simple_path(&t);
        } else {
            return None;
        }
    }
    None
}

fn get_pat_ty(ty: &syn::Type) -> Option<PatTy> {
    if let syn::Type::Path(p) = &ty {
        if let Some(ps) = p.path.segments.last() {
            let ps = ps.value();
            let id = &ps.ident;
            match &ps.arguments {
                syn::PathArguments::AngleBracketed(abga) => 
                    Some(PatTy {
                        inner_ty: get_inner_ty(&abga).unwrap().clone(),
                        ty: {
                            if id == "Alt" {
                                PatTy_::Alt
                            } else if id == "Seq" {
                                PatTy_::Seq
                            } else if id == "Opt" {
                                PatTy_::Opt
                            } else {
                                panic!("wrong type: {:?}", id)
                            }
                        }
                    }),
                _ => Some(PatTy {
                    inner_ty: id.clone(),
                    ty: PatTy_::Alt
                })
            }
        } else {
            None
        }
    } else {
        None
    }
}



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
    
    let pat_ty = get_pat_ty(&ty).unwrap();
    
    //print_hm(&get_named_subpattern_types(&parse_pattern.node, &pat_ty));
    //dbg!(pat_ty);

    let named_subpattern_types = get_named_subpattern_types(&parse_pattern.node, &pat_ty);

    let result_items = named_subpattern_types.iter().map(
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
            // TODO: implement others!
            _ => panic!("This is not implemented yet!!")
        }
    ).collect::<Vec<_>>();

    let init_items = named_subpattern_types.iter().map(
        |(k, v)| match v {
            ResTy::Elmt(e) => quote!( #k: None, ),
            ResTy::Opt(o) => match &**o {
                ResTy::Elmt(e) => quote!( #k: None, ),
                _ => panic!("This is not implemented yet!!")
            },
            ResTy::Seq(s) => match &**s {
                ResTy::Elmt(e) => quote!( #k: vec!(), ),
                _ => panic!("This is not implemented yet!!")
            },
            // TODO: implement others!
            _ => panic!("This is not implemented yet!!")
        }
    ).collect::<Vec<_>>();

    let pattern_ty = match &root_ty {
        Some(Ty::Alt) => quote!( Alt<'_, '_, pattern_tree::Expr<'_, '_, #struct_name<Ast>, Ast>, #struct_name<Ast>, <Ast as pattern_tree::MatchAssociations>::Expr> ),
        Some(Ty::Seq) => quote!( Seq<'_, '_, pattern_tree::Expr<'_, '_, #struct_name<Ast>, Ast>, #struct_name<Ast>, <Ast as pattern_tree::MatchAssociations>::Expr> ),
        Some(Ty::Opt) => quote!( Opt<'_, '_, pattern_tree::Expr<'_, '_, #struct_name<Ast>, Ast>, #struct_name<Ast>, <Ast as pattern_tree::MatchAssociations>::Expr> ),
        None => quote!( pattern_tree::Expr<'_, '_, #struct_name<Ast>, Ast> ),
    };

    let tokens = match root_ty {
        Some(root_ty) => to_tokens(&parse_pattern.node, &root_ty, &named_subpattern_types),
        None => to_tokens_node(&parse_pattern.node, &named_subpattern_types)
    };
    quote!(

        #[derive(Debug, Default)]
        pub struct #struct_name<'o, A>
        where A: pattern_tree::MatchAssociations {
            #(#result_items)*
        }

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
        
        
        fn #name (node: &<Ast as pattern_tree::MatchAssociations>::Expr) -> bool {
            let pattern: #pattern_ty = #tokens;
            //dbg!(pattern);

            let mut cx = #struct_name {
                #(#init_items)*
            };

            let (r, cx) = pattern.is_match(&mut cx, node);
            r
            //true
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
    quote!(pattern_tree::variants:: #ident ( #(#tokens),* ))
}

fn to_tokens_node(parse_tree: &ParseExpr, named_types: &HashMap<Ident, ResTy<Ident>>) -> proc_macro2::TokenStream {
    match parse_tree {
        ParseExpr::Node(ident, args) => {
            let tokens = node_to_tokens(ident, args, named_types);
            quote!(#tokens)
        },
        _ => panic!("Expected node")
    }
}


fn to_tokens_alt(parse_tree: &ParseExpr, named_types: &HashMap<Ident, ResTy<Ident>>) -> proc_macro2::TokenStream {
    match parse_tree {
        ParseExpr::Any => quote!(pattern_tree::matchers::Alt::Any),
        ParseExpr::Alt(a, b) => {
            let a_tokens = to_tokens_alt(a, named_types);
            let b_tokens = to_tokens_alt(b, named_types);
            quote!(pattern_tree::matchers::Alt::Alt(Box::new(#a_tokens), Box::new(#b_tokens)))
        },
        ParseExpr::Node(ident, args) => {
            let tokens = node_to_tokens(ident, args, named_types);
            quote!(pattern_tree::matchers::Alt::Elmt(Box::new(#tokens)))
        },
        ParseExpr::Lit(l) => {
            quote!(pattern_tree::matchers::Alt::Elmt(Box::new(#l)))
        },
        ParseExpr::Named(e, i) => {
            let e_tokens = to_tokens_alt(e, named_types);
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

fn to_tokens_opt(parse_tree: &ParseExpr, named_types: &HashMap<Ident, ResTy<Ident>>) -> proc_macro2::TokenStream {
    match parse_tree {
        ParseExpr::Any => quote!(pattern_tree::matchers::Opt::Any),
        ParseExpr::Alt(a, b) => {
            let a_tokens = to_tokens_opt(a, named_types);
            let b_tokens = to_tokens_opt(b, named_types);
            quote!(pattern_tree::matchers::Opt::Alt(Box::new(#a_tokens), Box::new(#b_tokens)))
        },
        ParseExpr::Node(ident, args) => {
            let tokens = node_to_tokens(ident, args, named_types);
            quote!(pattern_tree::matchers::Opt::Elmt(Box::new(#tokens)))
        },
        ParseExpr::Lit(l) => {
            quote!(pattern_tree::matchers::Opt::Elmt(Box::new(#l)))
        },
        ParseExpr::Named(e, i) => {
            let e_tokens = to_tokens_opt(e, named_types);
            quote!(
                pattern_tree::matchers::Opt::Named(
                    Box::new(#e_tokens), 
                    |cx, elmt| {cx.#i = Some(elmt); cx}
                )
            )
        },
        ParseExpr::Empty => quote!(pattern_tree::matchers::Opt::None),
        ParseExpr::Repeat(e, RepeatKind::Optional) => {
            let e_tokens = to_tokens_opt(e, named_types);
            quote!(pattern_tree::matchers::Opt::Alt(Box::new(#e_tokens), Box::new(pattern_tree::matchers::Opt::None)))
        },
        ParseExpr::Repeat(_, _) => 
            panic!("`*`, `+` and `{..}` arent't allowed when Opt<_> is expected"),
        _ => panic!("Seq isn't allowed when Opt<_> is expected")
    }
}

fn to_tokens_seq(parse_tree: &ParseExpr, named_types: &HashMap<Ident, ResTy<Ident>>) -> proc_macro2::TokenStream {
    match parse_tree {
        ParseExpr::Any => quote!(pattern_tree::matchers::Seq::Any),
        ParseExpr::Alt(a, b) => {
            let a_tokens = to_tokens_seq(a, named_types);
            let b_tokens = to_tokens_seq(b, named_types);
            quote!(pattern_tree::matchers::Seq::Alt(Box::new(#a_tokens), Box::new(#b_tokens)))
        },
        ParseExpr::Node(ident, args) => {
            let tokens = node_to_tokens(ident, args, named_types);
            quote!(pattern_tree::matchers::Seq::Elmt(Box::new(#tokens)))
        },
        ParseExpr::Lit(l) => {
            quote!(pattern_tree::matchers::Seq::Elmt(Box::new(#l)))
        },
        ParseExpr::Named(e, i) => {
            let ty = named_types.get(i).unwrap();
            let action = if let ResTy::Seq(s) = ty {
                quote!( cx.#i.push(elmt); )
            } else {
                quote!( cx.#i = Some(elmt); )
            };
            let e_tokens = to_tokens_seq(e, named_types);
            quote!(
                pattern_tree::matchers::Seq::Named(
                    Box::new(#e_tokens), 
                    |cx, elmt| {#action cx}
                )
            )
        },
        ParseExpr::Empty => quote!(pattern_tree::matchers::Seq::Empty),
        ParseExpr::Repeat(e, r) => {
            let e_tokens = to_tokens_seq(e, named_types);
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
            let a_tokens = to_tokens_seq(a, named_types);
            let b_tokens = to_tokens_seq(b, named_types);
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
