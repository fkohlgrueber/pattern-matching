#![allow(dead_code)]

#[derive(PartialEq, Debug, Clone, Copy)]
pub struct RepeatRange {
    pub start: usize,
    pub end: Option<usize>  // exclusive
}

impl RepeatRange {
    fn mul(&self, other: &RepeatRange) -> RepeatRange {
        RepeatRange {
            start: self.start * other.start,
            end: self.end.and_then(|s| other.end.map(|o| s * o))
        }
    }

    fn add(&self, other: &RepeatRange) -> RepeatRange {
        RepeatRange {
            start: self.start + other.start,
            end: self.end.and_then(|s| other.end.map(|o| s + o))
        }
    }

    fn max_range(&self, other: &RepeatRange) -> RepeatRange {
        RepeatRange {
            start: std::cmp::min(self.start, other.start),
            end: self.end.and_then(|s| other.end.map(|o| std::cmp::max(s, o)))
        }
    }
}

type Setter<'cx, 'o, Cx, O> = fn(&'cx mut Cx, &'o O) -> &'cx mut Cx;

#[derive(Debug)]
pub enum Alt<'cx, 'o, T, Cx, O> {
    Any,
    Elmt(Box<T>),
    Alt(Box<Self>, Box<Self>),
    Named(Box<Self>, Setter<'cx, 'o, Cx, O>)
}

#[derive(Debug)]
pub enum Seq<'cx, 'o, T, Cx, O> {
    Any,
    Empty,
    Elmt(Box<T>),
    Repeat(Box<Self>, RepeatRange),
    Seq(Box<Self>, Box<Self>),
    Alt(Box<Self>, Box<Self>),
    Named(Box<Self>, Setter<'cx, 'o, Cx, O>)
}

#[derive(Debug)]
pub enum Opt<'cx, 'o, T, Cx, O> {
    Any,  // anything, but not None
    Elmt(Box<T>),
    None,
    Alt(Box<Self>, Box<Self>),
    Named(Box<Self>, Setter<'cx, 'o, Cx, O>)
}

impl<'cx, 'o, T, Cx, O> Seq<'cx, 'o, T, Cx, O> {

    /// Returns the min / max repetitions that can match the Seq
    pub fn num_elmts_range(&self) -> RepeatRange {
        match self {
            Seq::Any => RepeatRange { start: 1, end: Some(2)},
            Seq::Empty => RepeatRange { start: 0, end: Some(1)},
            Seq::Elmt(_) => RepeatRange { start: 1, end: Some(2)},
            Seq::Repeat(e, r) => {
                let e_range = e.num_elmts_range();
                e_range.mul(r)
            },
            Seq::Seq(a, b) => {
                let a_range = a.num_elmts_range();
                let b_range = b.num_elmts_range();
                a_range.add(&b_range)
            },
            Seq::Alt(a, b) => {
                let a_range = a.num_elmts_range();
                let b_range = b.num_elmts_range();
                a_range.max_range(&b_range)
            },
            Seq::Named(e, _) => {
                e.num_elmts_range()
            }
        }
    }
}