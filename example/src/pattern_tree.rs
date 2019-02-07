#![allow(dead_code)]
use crate::matchers::*;

#[derive(Debug)]
pub enum Expr {
    Lit(Alt<Lit>),
    Array(Seq<Expr>),
    Test(Opt<Lit>)
}

#[derive(Debug)]
pub enum Lit {
    Char(Alt<char>),
    Bool(Alt<bool>),
    Int(Alt<u128>),
}

