use crate::{IsMatch, ReduceSelf};
use crate::pattern_tree::*;

// Dummy Ast Nodes

#[derive(Debug, PartialEq)]
pub enum DummyExpr {
    Lit(DummyLit),
    Array(Vec<DummyExpr>),
    Block(DummyBlock),
    If(Box<DummyExpr>, DummyBlock, Box<Option<DummyExpr>>)
    // IfLet not implemented
}

#[derive(Debug, PartialEq)]
pub enum DummyLit {
    Char(char),
    Bool(bool),
    Int(u128),
}

#[derive(Debug, PartialEq)]
pub enum DummyStmt {
    Expr(DummyExpr),
    Semi(DummyExpr),
}

#[derive(Debug, PartialEq)]
pub struct DummyBlock(pub Vec<DummyStmt>);

// Dummy Ast MatchAssociations

#[derive(Debug, PartialEq)]
pub struct DummyAst {}

pub mod variants {
    pub use super::DummyExpr::*;
    pub use super::DummyLit::*;
    pub use super::DummyStmt::*;
}

impl<'o> MatchAssociations<'o> for DummyAst {
    type Expr = DummyExpr;
    type Lit = DummyLit;
    type Bool = bool;
    type Char = char;
    type Int = u128;
    type Stmt = DummyStmt;
    type BlockType = DummyBlock;
}

// Dummy Ast IsMatch implementations

impl<'cx, 'o, Cx> IsMatch<'cx, 'o, Cx, DummyLit> for Lit<'cx, 'o, Cx, DummyAst> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o DummyLit) -> (bool, &'cx mut Cx) {
        match (self, other) {
            (Lit::Char(i), DummyLit::Char(j)) => i.is_match(cx, j),
            (Lit::Bool(i), DummyLit::Bool(j)) => i.is_match(cx, j),
            (Lit::Int(i), DummyLit::Int(j)) => i.is_match(cx, j),
            _ => (false, cx),
        }
    }
}

impl<'cx, 'o, Cx> IsMatch<'cx, 'o, Cx, DummyExpr> for Expr<'cx, 'o, Cx, DummyAst> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o DummyExpr) -> (bool, &'cx mut Cx) {
        match (self, other) {
            (Expr::Lit(i), DummyExpr::Lit(j)) => 
                i.is_match(cx, j),
            (Expr::Block_(i), DummyExpr::Block(j)) => 
                i.is_match(cx, j),
            (Expr::Array(i), DummyExpr::Array(j)) => 
                i.is_match(cx, j),
            (Expr::If(i_check, i_then, i_else), DummyExpr::If(j_check, j_then, j_else)) => {
                let (r_c, cx) = i_check.is_match(cx, j_check);
                let (r_t, cx) = i_then.is_match(cx, j_then);
                let (r_e, cx) = i_else.is_match(cx, j_else);
                (r_c && r_t && r_e, cx)
            },
            // IfLet not implemented
            _ => (false, cx),
        }
    }
}

impl<'cx, 'o, Cx> IsMatch<'cx, 'o, Cx, DummyStmt> for Stmt<'cx, 'o, Cx, DummyAst> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o DummyStmt) -> (bool, &'cx mut Cx) {
        match (self, other) {
            (Stmt::Expr(i), DummyStmt::Expr(j)) => i.is_match(cx, j),
            (Stmt::Semi(i), DummyStmt::Semi(j)) => i.is_match(cx, j),
            _ => (false, cx),
        }
    }
}

impl<'cx, 'o, Cx> IsMatch<'cx, 'o, Cx, DummyBlock> for BlockType<'cx, 'o, Cx, DummyAst> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o DummyBlock) -> (bool, &'cx mut Cx) {
        match self {
            BlockType::Block(e) => e.is_match(cx, &other.0)
        }
    }
}

impl ReduceSelf for DummyExpr {}
impl ReduceSelf for DummyStmt {}