
use crate::pattern_tree::*;
use crate::{IsMatch, Reduce, PatternTreeNode, MatchAssociations};
use syntax::ast;

#[derive(Debug)]
pub struct Ast {}

impl MatchAssociations for Ast {
    type Expr = ast::Expr;
    type Lit = ast::Lit;
    type Bool = bool;
    type Char = char;
    type Int = u128;
    type Stmt = ast::Stmt;
    type BlockType = ast::Block;
}

impl<'cx, 'o, Cx> IsMatch<'cx, 'o, Cx, ast::LitKind> for Lit<'cx, 'o, Cx, Ast> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o ast::LitKind) -> (bool, &'cx mut Cx) {
        match (self, other) {
            (Lit::Char(i), ast::LitKind::Char(j)) => i.is_match(cx, j),
            (Lit::Bool(i), ast::LitKind::Bool(j)) => i.is_match(cx, j),
            (Lit::Int(i), ast::LitKind::Int(j, _)) => i.is_match(cx, j),
            _ => (false, cx),
        }
    }
}

impl<'cx, 'o, Cx> IsMatch<'cx, 'o, Cx, ast::ExprKind> for Expr<'cx, 'o, Cx, Ast> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o ast::ExprKind) -> (bool, &'cx mut Cx) {
        match (self, other) {
            (Expr::Lit(i), ast::ExprKind::Lit(j)) => 
                i.is_match(cx, j),
            (Expr::Block_(i), ast::ExprKind::Block(j, _label)) => 
                i.is_match(cx, j),
            (Expr::Array(i), ast::ExprKind::Array(j)) => 
                i.is_match(cx, j),
            (Expr::If(i_check, i_then, i_else), ast::ExprKind::If(j_check, j_then, j_else)) => {
                let (r_c, cx) = i_check.is_match(cx, j_check);
                let (r_t, cx) = i_then.is_match(cx, j_then);
                let (r_e, cx) = i_else.is_match(cx, j_else);
                (r_c && r_t && r_e, cx)
            },
            (Expr::IfLet(i_block, i_else), ast::ExprKind::IfLet(_pattern, _check, j_block, j_else)) => {
                // TODO: also check pattern and expr
                let (r_b, cx) = i_block.is_match(cx, j_block);
                let (r_e, cx) = i_else.is_match(cx, j_else);
                (r_b && r_e, cx)
            },
            _ => (false, cx),
        }
    }
}

impl<'cx, 'o, Cx> IsMatch<'cx, 'o, Cx, ast::Expr> for Expr<'cx, 'o, Cx, Ast> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o ast::Expr) -> (bool, &'cx mut Cx) {
        self.is_match(cx, &other.node)
    }
}

impl<'cx, 'o, Cx> IsMatch<'cx, 'o, Cx, ast::StmtKind> for Stmt<'cx, 'o, Cx, Ast> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o ast::StmtKind) -> (bool, &'cx mut Cx) {
        match (self, other) {
            (Stmt::Expr(i), ast::StmtKind::Expr(j)) => i.is_match(cx, j),
            (Stmt::Semi(i), ast::StmtKind::Semi(j)) => i.is_match(cx, j),
            _ => (false, cx),
        }
    }
}

impl<'cx, 'o, Cx> IsMatch<'cx, 'o, Cx, ast::Stmt> for Stmt<'cx, 'o, Cx, Ast> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o ast::Stmt) -> (bool, &'cx mut Cx) {
        self.is_match(cx, &other.node)
    }
}

impl<'cx, 'o, Cx> IsMatch<'cx, 'o, Cx, ast::Block> for BlockType<'cx, 'o, Cx, Ast> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o ast::Block) -> (bool, &'cx mut Cx) {
        match self {
            BlockType::Block(e) => e.is_match(cx, &other.stmts)
        }
    }
}

impl<'cx, 'o, Cx> IsMatch<'cx, 'o, Cx, ast::Lit> for Lit<'cx, 'o, Cx, Ast> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o ast::Lit) -> (bool, &'cx mut Cx) {
        self.is_match(cx, &other.node)
    }
}

impl<'cx, 'o, Cx, A> PatternTreeNode for Lit<'cx, 'o, Cx, A> where A: MatchAssociations {}
impl<'cx, 'o, Cx, A> PatternTreeNode for Expr<'cx, 'o, Cx, A> where A: MatchAssociations {}
impl<'cx, 'o, Cx, A> PatternTreeNode for Stmt<'cx, 'o, Cx, A> where A: MatchAssociations {}
impl<'cx, 'o, Cx, A> PatternTreeNode for BlockType<'cx, 'o, Cx, A> where A: MatchAssociations {}


impl Reduce for syntax::ast::Stmt {
    type Target = syntax::ast::Stmt;

    fn reduce(&self) -> &Self::Target {
        self
    }
}

impl<T> Reduce for syntax::ptr::P<T> {
    type Target = T;

    fn reduce(&self) -> &Self::Target {
        &*self
    }
}