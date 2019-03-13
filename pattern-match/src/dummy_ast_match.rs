use crate::{IsMatch, ReduceSelf};
use crate::pattern_tree::*;

// Dummy Ast Nodes

#[derive(Debug, PartialEq, Clone)]
pub enum DummyExpr {
    Lit(DummyLit),
    Array(Vec<DummyExpr>),
    Block(DummyBlock),
    If(Box<DummyExpr>, DummyBlock, Box<Option<DummyExpr>>)
    // IfLet not implemented
}

#[derive(Debug, PartialEq, Clone)]
pub enum DummyLit {
    Char(char),
    Bool(bool),
    Int(u128, DummyLitIntType),
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
    type Bool = bool;
    type Char = char;
    type Int = u128;
    type Stmt = DummyStmt;
    type BlockType = DummyBlock;
    type LitIntType = DummyLitIntType;
    type IntTy = DummyIntTy;
    type UintTy = DummyUintTy;
}

// Dummy Ast IsMatch implementations

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, DummyLit> for Lit<'cx, 'o, Cx, DummyAst> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o DummyLit) -> (bool, &'cx mut Cx) {
        match (self, other) {
            (Lit::Char(i), DummyLit::Char(j)) => i.is_match(cx, j),
            (Lit::Bool(i), DummyLit::Bool(j)) => i.is_match(cx, j),
            (Lit::Int(i, i_suffix), DummyLit::Int(j, j_suffix)) => {
                let cx_orig = cx.clone();
                let (r, cx) = i.is_match(cx, j);
                let (r_suffix, cx) = i_suffix.is_match(cx, j_suffix);
                if r && r_suffix {
                    (true, cx)
                } else {
                    *cx = cx_orig;
                    (false, cx)
                }
            },
            _ => (false, cx),
        }
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, DummyExpr> for Expr<'cx, 'o, Cx, DummyAst> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o DummyExpr) -> (bool, &'cx mut Cx) {
        match (self, other) {
            (Expr::Lit(i), DummyExpr::Lit(j)) => 
                i.is_match(cx, j),
            (Expr::Block_(i), DummyExpr::Block(j)) => 
                i.is_match(cx, j),
            (Expr::Array(i), DummyExpr::Array(j)) => 
                i.is_match(cx, j),
            (Expr::If(i_check, i_then, i_else), DummyExpr::If(j_check, j_then, j_else)) => {
                let cx_orig = cx.clone();
                let (r_c, cx) = i_check.is_match(cx, j_check);
                let (r_t, cx) = i_then.is_match(cx, j_then);
                let (r_e, cx) = i_else.is_match(cx, j_else);
                if r_c && r_t && r_e {
                    (true, cx)
                } else {
                    *cx = cx_orig;
                    (false, cx)
                }
            },
            // IfLet not implemented
            _ => (false, cx),
        }
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, DummyStmt> for Stmt<'cx, 'o, Cx, DummyAst> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o DummyStmt) -> (bool, &'cx mut Cx) {
        match (self, other) {
            (Stmt::Expr(i), DummyStmt::Expr(j)) |
            (Stmt::Semi(i), DummyStmt::Semi(j)) => i.is_match(cx, j),
            _ => (false, cx),
        }
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, DummyBlock> for BlockType<'cx, 'o, Cx, DummyAst> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o DummyBlock) -> (bool, &'cx mut Cx) {
        match self {
            BlockType::Block(e) => e.is_match(cx, &other.0)
        }
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, DummyLitIntType> for LitIntType<'cx, 'o, Cx, DummyAst> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o DummyLitIntType) -> (bool, &'cx mut Cx) {
        match (self, other) {
            (LitIntType::Signed(i), DummyLitIntType::Signed(j)) => i.is_match(cx, j),
            (LitIntType::Unsigned(i), DummyLitIntType::Unsigned(j)) => i.is_match(cx, j),
            (LitIntType::Unsuffixed, DummyLitIntType::Unsuffixed) => (true, cx),
            _ => (false, cx),
        }
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, DummyIntTy> for IntTy {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o DummyIntTy) -> (bool, &'cx mut Cx) {
        match (self, other) {
            (IntTy::Isize, DummyIntTy::Isize) |
            (IntTy::I8, DummyIntTy::I8) |
            (IntTy::I16, DummyIntTy::I16) |
            (IntTy::I32, DummyIntTy::I32) |
            (IntTy::I64, DummyIntTy::I64) |
            (IntTy::I128, DummyIntTy::I128) => (true, cx),
            _ => (false, cx),
        }
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, DummyUintTy> for UintTy {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o DummyUintTy) -> (bool, &'cx mut Cx) {
        match (self, other) {
            (UintTy::Usize, DummyUintTy::Usize) |
            (UintTy::U8, DummyUintTy::U8) |
            (UintTy::U16, DummyUintTy::U16) |
            (UintTy::U32, DummyUintTy::U32) |
            (UintTy::U64, DummyUintTy::U64) |
            (UintTy::U128, DummyUintTy::U128) => (true, cx),
            _ => (false, cx),
        }
    }
}

impl ReduceSelf for DummyExpr {}
impl ReduceSelf for DummyStmt {}