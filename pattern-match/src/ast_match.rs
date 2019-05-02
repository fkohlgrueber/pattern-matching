
use crate::pattern_tree::*;
use crate::{IsMatch, Reduce, ReduceSelf};
use syntax::ast;

#[derive(Debug, Clone)]
pub struct Ast {}

impl<'o> MatchAssociations<'o> for Ast {
    type Expr = ast::Expr;
    type Lit = ast::Lit;
    type Stmt = ast::Stmt;
    type BlockType = ast::Block;
    type LitIntType = ast::LitIntType;
    type IntTy = ast::IntTy;
    type UintTy = ast::UintTy;
}

use is_match_macro::derive_is_match_impl;

derive_is_match_impl!{
    Expr <> ast::ExprKind => Ast => {
        Lit(l) <> Lit(l)
        Block_(b) <> Block(b, _label)
        Array(a) <> Array(a)
        If(check, then, else_) <> If(check, then, else_)
        IfLet(then, else_) <> IfLet(_pattern, _check, then, else_)
    }
}

derive_is_match_impl!{
    Stmt <> ast::StmtKind => Ast => {
        Expr(e) <> Expr(e)
        Semi(s) <> Semi(s)
    }
}

derive_is_match_impl!{
    Lit <> ast::LitKind => Ast => {
        Char(i) <> Char(i)
        Bool(i) <> Bool(i)
        Int(n, suffix) <> Int(n, suffix)
    }
}

derive_is_match_impl!{
    LitIntType <> ast::LitIntType => Ast => {
        Signed(i) <> Signed(i)
        Unsigned(i) <> Unsigned(i)
        Unsuffixed <> Unsuffixed
    }
}

derive_is_match_impl!{
    IntTy <> ast::IntTy => {
        Isize <> Isize
        I8 <> I8
        I16 <> I16
        I32 <> I32
        I64 <> I64
        I128 <> I128
    }
}

derive_is_match_impl!{
    UintTy <> ast::UintTy => {
        Usize <> Usize
        U8 <> U8
        U16 <> U16
        U32 <> U32
        U64 <> U64
        U128 <> U128
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, ast::Expr> for Expr<'cx, 'o, Cx, Ast> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o ast::Expr) -> (bool, &'cx mut Cx) {
        self.is_match(cx, &other.node)
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, ast::Stmt> for Stmt<'cx, 'o, Cx, Ast> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o ast::Stmt) -> (bool, &'cx mut Cx) {
        self.is_match(cx, &other.node)
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, ast::Block> for BlockType<'cx, 'o, Cx, Ast> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o ast::Block) -> (bool, &'cx mut Cx) {
        match self {
            BlockType::Block(e) => e.is_match(cx, &other.stmts)
        }
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, ast::Lit> for Lit<'cx, 'o, Cx, Ast> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o ast::Lit) -> (bool, &'cx mut Cx) {
        self.is_match(cx, &other.node)
    }
}

impl ReduceSelf for syntax::ast::Stmt {}

impl<T> Reduce for syntax::ptr::P<T> {
    type Target = T;

    fn reduce(&self) -> &Self::Target {
        &*self
    }
}