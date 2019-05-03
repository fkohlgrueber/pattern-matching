use crate::{IsMatch, ReduceSelf};
use crate::pattern_tree_rust::*;

// Dummy Ast Nodes

#[derive(Debug, PartialEq, Clone)]
pub enum DummyExpr {
    Lit(DummyLit),
    Array(Vec<DummyExpr>),
    Block(DummyBlock),
    If(Box<DummyExpr>, DummyBlock, Box<Option<DummyExpr>>),
    IfLet(DummyBlock, Box<Option<DummyExpr>>)
}

#[derive(Debug, PartialEq, Clone)]
pub enum DummyLit {
    Char(char),
    Bool(bool),
    Int(u128, DummyLitIntType),
    Str(String)
}

#[derive(Debug, PartialEq, Clone)]
pub enum DummyStmt {
    Expr(DummyExpr),
    Semi(DummyExpr),
}

#[derive(Debug, PartialEq, Clone)]
pub struct DummyBlock(pub Vec<DummyStmt>);

#[derive(Debug, PartialEq, Clone)]
pub enum DummyLitIntType {
    Signed(DummyIntTy),
    Unsigned(DummyUintTy),
    Unsuffixed,
}

#[derive(Debug, PartialEq, Clone)]
pub enum DummyIntTy
{
    Isize,
    I8,
    I16,
    I32,
    I64,
    I128,
}

#[derive(Debug, PartialEq, Clone)]
pub enum DummyUintTy {
    Usize,
    U8,
    U16,
    U32,
    U64,
    U128,
}

// Dummy Ast MatchAssociations

#[derive(Debug, PartialEq, Clone)]
pub struct DummyAst {}

pub mod variants {
    pub use super::DummyExpr::*;
    pub use super::DummyLit::*;
    pub use super::DummyStmt::*;
    pub use super::DummyLitIntType::*;
    pub use super::DummyIntTy::*;
    pub use super::DummyUintTy::*;
}

impl<'o> MatchAssociations<'o> for DummyAst {
    type Expr = DummyExpr;
    type Lit = DummyLit;
    type Stmt = DummyStmt;
    type BlockType = DummyBlock;
    type LitIntType = DummyLitIntType;
    type IntTy = DummyIntTy;
    type UintTy = DummyUintTy;
    type Symbol = String;
}

// Dummy Ast IsMatch implementations

use is_match_macro::derive_is_match_impl;

derive_is_match_impl!{
    Expr <> DummyExpr => DummyAst => {
        Lit(l) <> Lit(l)
        Block_(b) <> Block(b)
        Array(a) <> Array(a)
        If(check, then, else_) <> If(check, then, else_)
        IfLet(then, else_) <> IfLet(then, else_)
    }
}

derive_is_match_impl!{
    LitIntType <> DummyLitIntType => DummyAst => {
        Signed(i) <> Signed(i)
        Unsigned(i) <> Unsigned(i)
        Unsuffixed <> Unsuffixed
    }
}

derive_is_match_impl!{
    Lit <> DummyLit => DummyAst => {
        Char(i) <> Char(i)
        Bool(i) <> Bool(i)
        Int(n, suffix) <> Int(n, suffix)
        Str(s) <> Str(s)
    }
}


impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, String> for &'static str {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o String) -> (bool, &'cx mut Cx) {
        (self == &other.as_str(), cx)
    }
}

derive_is_match_impl!{
    Stmt <> DummyStmt => DummyAst => {
        Expr(e) <> Expr(e)
        Semi(s) <> Semi(s)
    }
}

derive_is_match_impl!{
    IntTy <> DummyIntTy => {
        Isize <> Isize
        I8 <> I8
        I16 <> I16
        I32 <> I32
        I64 <> I64
        I128 <> I128
    }
}

derive_is_match_impl!{
    UintTy <> DummyUintTy => {
        Usize <> Usize
        U8 <> U8
        U16 <> U16
        U32 <> U32
        U64 <> U64
        U128 <> U128
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, DummyBlock> for BlockType<'cx, 'o, Cx, DummyAst> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o DummyBlock) -> (bool, &'cx mut Cx) {
        match self {
            BlockType::Block(e) => e.is_match(cx, &other.0)
        }
    }
}

impl ReduceSelf for DummyExpr {}
impl ReduceSelf for DummyStmt {}