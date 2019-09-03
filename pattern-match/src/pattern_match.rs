#![feature(rustc_private)]
#![feature(box_syntax)]

extern crate syntax;
extern crate rustc_data_structures;

pub mod ast_match;
pub mod ast_match_rust_mini;
pub mod pattern_tree_rust;
pub mod pattern_tree_rust_mini;
pub mod pattern_tree_meta;
pub mod meta_match;
pub mod dummy_ast_match;

use itertools::Itertools;
use itertools::repeat_n;

pub use pattern_tree;
use pattern_tree::matchers::*;

impl IsMatchEquality for u128 {}
impl IsMatchEquality for char {}
impl IsMatchEquality for bool {}
impl IsMatchEquality for &'static str {}

// Main trait for matching
pub trait IsMatch<'cx, 'o, Cx: Clone, O: ?Sized> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o O) -> (bool, &'cx mut Cx);
}

// Trait for types that can be matched by their equality
pub trait IsMatchEquality: PartialEq {}

impl<'cx, 'o, Cx: Clone, T> IsMatch<'cx, 'o, Cx, T> for T 
where T: IsMatchEquality {
    fn is_match(&self, cx: &'cx mut Cx, other: &Self) -> (bool, &'cx mut Cx) {
        (self == other, cx)
    }
}

impl<'cx, 'o, T, U, Cx: Clone> IsMatch<'cx, 'o, Cx, U> for Alt<'cx, 'o, T, Cx, U>
where T: IsMatch<'cx, 'o, Cx, U> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o U) -> (bool, &'cx mut Cx) {
        match self {
            Alt::Any => (true, cx),
            Alt::Elmt(e) => e.is_match(cx, other),
            Alt::Named(e, f) => {
                let (r, mut cx) = e.is_match(cx, other);
                if r { 
                    cx = f(cx, other);
                }
                (r, cx)
            },
            Alt::Alt(i, j) => {
                let (r_i, cx) = i.is_match(cx, other);
                // early return if first alternative matched
                if r_i { 
                    return (r_i, cx); 
                }
                j.is_match(cx, other)
            }
        }
    }
}

impl<'cx, 'o, T, U, V, Cx: Clone> IsMatch<'cx, 'o, Cx, [V]> for Seq<'cx, 'o, T, Cx, U>
where 
    T: IsMatch<'cx, 'o, Cx, U>,
    V: Reduce<Target=U>
{
    fn is_match(&self, cx: &'cx mut Cx, other: &'o [V]) -> (bool, &'cx mut Cx) {
        let mut cx = cx;
        match self {
            Seq::Any => (other.len() == 1, cx),
            Seq::Elmt(e) => {
                if other.len() != 1 { return (false, cx); }
                e.is_match(cx, &other[0].reduce())
            },
            Seq::Named(e, f) => {
                let (r, mut cx) = e.is_match(cx, other);
                if r {
                    for o in other {
                        cx = f(cx, o.reduce());
                    }
                }
                (r, cx)
            },
            Seq::Alt(i, j) => {
                let (r_i, cx) = i.is_match(cx, other);
                // early return if first alternative matched
                if r_i { 
                    return (r_i, cx); 
                }
                j.is_match(cx, other)
            },
            Seq::Empty => (other.is_empty(), cx),
            Seq::Repeat(e, r) => {
                // TODO: cx / cx_orig are cloned more often than necessary. 
                //       Fix this (also applies to Seq::Seq)
                let cx_orig = cx.clone();
                let e_range = e.num_elmts_range();
                let e_range = e_range.start..e_range.end.unwrap_or(other.len()+1);

                if r.start == 0 && other.is_empty() {
                    return (true, cx);
                }

                for i in r.start..r.end.unwrap_or(other.len()+1) {
                    
                    // TODO: this can be done more efficiently
                    let iterators = repeat_n(e_range.clone(), i)
                        .multi_cartesian_product()
                        .filter(|x| x.iter().sum::<usize>() == other.len());

                    'outer: for vals in iterators {
                        *cx = cx_orig.clone();
                        let mut skip = 0;
                        for v in &vals {
                            let (r_e, cx_tmp) = e.is_match(cx, &other[skip..skip+v]);
                            cx = cx_tmp;
                            if !r_e {
                                continue 'outer;
                            }
                            skip += v;
                        }
                        return (true, cx);
                    }
                }
                *cx = cx_orig;
                (false, cx)
            },
            Seq::Seq(a, b) => {
                let cx_orig = cx.clone();
                let range = a.num_elmts_range();
                for i in range.start..range.end.unwrap_or(other.len()+1) {
                    *cx = cx_orig.clone();
                    if i > other.len() {
                        break;
                    }
                    let (l, r) = other.split_at(i);
                    let (r_a, cx_tmp) = a.is_match(cx, l);
                    cx = cx_tmp;
                    if r_a {
                        let (r_b, cx_tmp) = b.is_match(cx, r);
                        cx = cx_tmp;
                        if r_b {
                            return (true, cx);
                        }
                    }
                }
                *cx = cx_orig;
                (false, cx)
            },
            
        }
    }
}

impl<'cx, 'o, T, U, V, Cx: Clone> IsMatch<'cx, 'o, Cx, Vec<V>> for Seq<'cx, 'o, T, Cx, U>
where 
    T: IsMatch<'cx, 'o, Cx, U>,
    V: Reduce<Target=U>
{
    fn is_match(&self, cx: &'cx mut Cx, other: &'o Vec<V>) -> (bool, &'cx mut Cx) {
        self.is_match(cx, &other[..])
    }
}


impl<'cx, 'o, T, U, V, Cx: Clone> IsMatch<'cx, 'o, Cx, Option<V>> for Opt<'cx, 'o, T, Cx, U>
where 
    T: IsMatch<'cx, 'o, Cx, U>,
    V: Reduce<Target=U>
{
    fn is_match(&self, cx: &'cx mut Cx, other: &'o Option<V>) -> (bool, &'cx mut Cx) {
        
        match self {
            Opt::Any => (other.is_some(), cx),
            Opt::Elmt(e) => match other {
                Some(other) => e.is_match(cx, other.reduce()),
                None => (false, cx)
            },
            Opt::Named(e, f) => {
                let (r, mut cx) = e.is_match(cx, other);
                if r {
                    if let Some(o) = other {
                        cx = f(cx, o.reduce())
                    }
                }
                (r, cx)
            },
            Opt::Alt(i, j) => {
                let (r_i, cx) = i.is_match(cx, other);
                // early return if first alternative matched
                if r_i { 
                    return (r_i, cx); 
                }
                j.is_match(cx, other)
            },
            Opt::None => (other.is_none(), cx),
        }
    }
}

pub trait Reduce {
    type Target;

    fn reduce(&self) -> &Self::Target;
}

pub trait ReduceSelf {}

impl<T> Reduce for T 
where T: ReduceSelf {
    type Target = Self;

    fn reduce(&self) -> &Self::Target {
        self
    }
}