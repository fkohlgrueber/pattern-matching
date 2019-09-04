use crate::pattern_tree_rust::*;
use crate::{IsMatch, Reduce, ReduceSelf};
use rustc::hir;
use syntax::ast;

use is_match_macro::derive_is_match_impl;

#[derive(Debug, Clone)]
pub struct Hir {}

impl<'o> MatchAssociations<'o> for Hir {
    type Expr = hir::Expr;
    type Lit = hir::Lit;
    type Stmt = hir::Stmt;
    type BlockType = hir::Block;
    type LitIntType = ast::LitIntType;
    type IntTy = ast::IntTy;
    type UintTy = ast::UintTy;
    type Symbol = syntax::source_map::symbol::Symbol;
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, hir::Expr> for Expr<'cx, 'o, Cx, Hir> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o hir::Expr) -> (bool, &'cx mut Cx) {
        self.is_match(cx, &other.node)
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, hir::ExprKind> for Expr<'cx, 'o, Cx, Hir> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o hir::ExprKind) -> (bool, &'cx mut Cx) {
        match (self, other) {
            // Expr::Lit
            (Expr::Lit(l_a), hir::ExprKind::Lit(l_b)) => l_a.is_match(cx, l_b),
            (Expr::Array(a), hir::ExprKind::Array(b)) => a.is_match(cx, b),
                        
            // Expr::If doesn't exist in HIR

            (Expr::Block_(a), hir::ExprKind::Block(b, _label)) => a.is_match(cx, b),
            _ => (false, cx)
        }
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, hir::Lit> for Lit<'cx, 'o, Cx, Hir> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o hir::Lit) -> (bool, &'cx mut Cx) {
        self.is_match(cx, &other.node)
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, hir::Block> for BlockType<'cx, 'o, Cx, Hir> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o hir::Block) -> (bool, &'cx mut Cx) {
        match self {
            BlockType::Block(e) => e.is_match(cx, &other.stmts)
        }
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, hir::Stmt> for Stmt<'cx, 'o, Cx, Hir> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o hir::Stmt) -> (bool, &'cx mut Cx) {
        self.is_match(cx, &other.node)
    }
}


derive_is_match_impl!{
    Lit <> ast::LitKind => Hir => {
        Char(i) <> Char(i)
        Bool(i) <> Bool(i)
        Int(n, suffix) <> Int(n, suffix)
        Str(s) <> Str(s, _style)
    }
}

derive_is_match_impl!{
    LitIntType <> ast::LitIntType => Hir => {
        Signed(i) <> Signed(i)
        Unsigned(i) <> Unsigned(i)
        Unsuffixed <> Unsuffixed
    }
}


derive_is_match_impl!{
    Stmt <> hir::StmtKind => Hir => {
        Expr(e) <> Expr(e)
        Semi(s) <> Semi(s)
    }
}

impl ReduceSelf for hir::Stmt {}
impl ReduceSelf for hir::Expr {}

impl<T: ?Sized> Reduce for hir::ptr::P<T> {
    type Target = T;

    fn reduce(&self) -> &Self::Target {
        &**self
    }
}