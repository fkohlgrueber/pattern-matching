use common::Ty;
use crate::parse::ParseTree;
use crate::parse::RepeatKind;
use std::collections::HashMap;
use syn::Ident;
use crate::PatTy;
use pattern_match::pattern_tree::TYPES;
use std::cmp::max;


pub(crate) fn get_named_subpattern_types(input: &ParseTree, ty: &Ident) -> HashMap<Ident, PatTy> {
    match input {
        ParseTree::Node(id, args) => {
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
        ParseTree::Alt(a, b) => {
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
        ParseTree::Named(e, id) => {
            let mut h = get_named_subpattern_types(e, ty);
            
            // inner type (e.g. a pattern_tree node) is provided by parameter (top-down)
            let inner_ty = ty;

            // repeat type (single, optional, multiple) is determined by looking at own children (bottom-up)
            let repeat_type = get_repeat_type(e);
            
            h.insert(id.clone(), PatTy { ty: repeat_type, inner_ty: inner_ty.clone()});
            h
        },
        ParseTree::Lit(_l) => HashMap::new(),
        ParseTree::Any => HashMap::new(),
        
        ParseTree::Empty => HashMap::new(),
        ParseTree::Seq(a, b) => {
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
        ParseTree::Repeat(e, _r) => get_named_subpattern_types(e, ty),
    }
}

/// Returns how many elements might be captured by the PatternTree `p`.
/// 
/// Alt: Exactly one occurrance
/// Opt: At most one occurrance
/// Seq: Any number of occurrances
fn get_repeat_type(p: &ParseTree) -> Ty {
    match p {
        ParseTree::Any => Ty::Alt,
        ParseTree::Empty => Ty::Seq,
        ParseTree::Lit(_) => Ty::Alt,
        ParseTree::Node(_, _) => Ty::Alt,
        ParseTree::Alt(a, b) => max(get_repeat_type(a), get_repeat_type(b)),
        ParseTree::Seq(_, _) => Ty::Seq,
        ParseTree::Repeat(e, r) => {
            let c = get_repeat_type(e);
            match (c, r) {
                (Ty::Alt, RepeatKind::Optional) => Ty::Opt,
                (Ty::Opt, RepeatKind::Optional) => Ty::Opt,
                _ => Ty::Seq
            }
        },
        ParseTree::Named(e, _) => get_repeat_type(e)
    }
}
