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
    Alt(Box<Alt<T>>, Box<Alt<T>>),
    Named(Box<Alt<T>>, String)
}

#[derive(Debug)]
pub enum Seq<T> {
    Any,
    Empty,
    Elmt(Box<T>),
    Repeat(Box<Seq<T>>, RepeatRange),
    Seq(Box<Seq<T>>, Box<Seq<T>>),
    Alt(Box<Seq<T>>, Box<Seq<T>>),
    Named(Box<Seq<T>>, String)
}

#[derive(Debug)]
pub enum Opt<T> {
    Any,  // anything, but not None
    Elmt(T),
    None,
    Alt(Box<Opt<T>>, Box<Opt<T>>),
    Named(Box<Opt<T>>, String)
}