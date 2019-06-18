
use crate::pattern_tree_rust_mini::*;
use crate::IsMatch;
use syntax::ast;

use is_match_macro::derive_is_match_impl;

#[derive(Debug, Clone)]
pub struct AstMini {}

impl<'o> MatchAssociations<'o> for AstMini {
    type Expr = ast::Expr;
    type Lit = ast::Lit;
    type Stmt = ast::Stmt;
    type BlockType = ast::Block;
}

derive_is_match_impl!{
    Expr <> ast::ExprKind => AstMini => {
        Lit(l) <> Lit(l)
        Array(a) <> Array(a)
        If(check, then, else_) <> If(check, then, else_)
        Block_(b) <> Block(b, _label)
    }
}

derive_is_match_impl!{
    Stmt <> ast::StmtKind => AstMini => {
        Expr(e) <> Expr(e)
        Semi(s) <> Semi(s)
    }
}

derive_is_match_impl!{
    Lit <> ast::LitKind => => {
        Char(i) <> Char(i)
        Bool(i) <> Bool(i)
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, ast::Block> for BlockType<'cx, 'o, Cx, AstMini> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o ast::Block) -> (bool, &'cx mut Cx) {
        match self {
            BlockType::Block(e) => e.is_match(cx, &other.stmts)
        }
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, ast::Expr> for Expr<'cx, 'o, Cx, AstMini> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o ast::Expr) -> (bool, &'cx mut Cx) {
        self.is_match(cx, &other.node)
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, ast::Stmt> for Stmt<'cx, 'o, Cx, AstMini> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o ast::Stmt) -> (bool, &'cx mut Cx) {
        self.is_match(cx, &other.node)
    }
}


impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, ast::Lit> for Lit<'cx, 'o, Cx> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o ast::Lit) -> (bool, &'cx mut Cx) {
        self.is_match(cx, &other.node)
    }
}
