#![allow(dead_code)]

#[derive(PartialEq, Debug, Clone, Copy)]
pub struct RepeatRange {
    pub start: usize,
    pub end: Option<usize>  // exclusive
}

#[derive(Debug)]
pub enum Alt<T> {
    Any,
    Elmt(Box<T>),
    Alt(Box<Self>, Box<Self>),
    Named(Box<Self>, String)
}

#[derive(Debug)]
pub enum Seq<T> {
    Any,
    Empty,
    Elmt(Box<T>),
    Repeat(Box<Self>, RepeatRange),
    Seq(Box<Self>, Box<Self>),
    Alt(Box<Self>, Box<Self>),
    Named(Box<Self>, String)
}

#[derive(Debug)]
pub enum Opt<T> {
    Any,  // anything, but not None
    Elmt(Box<T>),
    None,
    Alt(Box<Self>, Box<Self>),
    Named(Box<Self>, String)
}

impl<T> Seq<T> {
    pub fn num_elmts_range(&self) -> RepeatRange {
        match self {
            Seq::Any => RepeatRange { start: 1, end: Some(2)},
            Seq::Empty => RepeatRange { start: 0, end: Some(1)},
            Seq::Elmt(_) => RepeatRange { start: 1, end: Some(2)},
            Seq::Repeat(e, r) => {
                let e_range = e.num_elmts_range();
                RepeatRange {
                    start: e_range.start * r.start,
                    end: match (e_range.end, r.end) {
                        (Some(a), Some(b)) => Some(a*b),
                        _ => None
                    }
                }
            },
            Seq::Seq(a, b) => {
                let a_range = a.num_elmts_range();
                let b_range = b.num_elmts_range();
                RepeatRange {
                    start: a_range.start + b_range.start,
                    end: match (a_range.end, b_range.end) {
                        (Some(a), Some(b)) => Some(a+b),
                        _ => None
                    }
                }
            },
            Seq::Alt(a, b) => {
                let a_range = a.num_elmts_range();
                let b_range = b.num_elmts_range();
                RepeatRange {
                    start: std::cmp::min(a_range.start, b_range.start),
                    end: match (a_range.end, b_range.end) {
                        (Some(a), Some(b)) => Some(std::cmp::max(a, b)),
                        _ => None
                    }
                }
            },
            Seq::Named(e, _) => {
                e.num_elmts_range()
            }
        }
    }
}