
use crate::pattern_tree::*;
use crate::{IsMatch, Reduce, ReduceSelf};
use syntax::ast;

#[derive(Debug, Clone)]
pub struct Ast {}

impl<'o> MatchAssociations<'o> for Ast {
    type Expr = ast::Expr;
    type Lit = ast::Lit;
    type Bool = bool;
    type Char = char;
    type Int = u128;
    type Stmt = ast::Stmt;
    type BlockType = ast::Block;
    type LitIntType = ast::LitIntType;
    type IntTy = ast::IntTy;
    type UintTy = ast::UintTy;
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, ast::LitKind> for Lit<'cx, 'o, Cx, Ast> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o ast::LitKind) -> (bool, &'cx mut Cx) {
        match (self, other) {
            (Lit::Char(i), ast::LitKind::Char(j)) => i.is_match(cx, j),
            (Lit::Bool(i), ast::LitKind::Bool(j)) => i.is_match(cx, j),
            (Lit::Int(i, i_suffix), ast::LitKind::Int(j, j_suffix)) => {
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
            // no match otherwise
            (Lit::Char(_), _) |
            (Lit::Bool(_), _) |
            (Lit::Int(_, _), _) => (false, cx)
        }
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, ast::ExprKind> for Expr<'cx, 'o, Cx, Ast> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o ast::ExprKind) -> (bool, &'cx mut Cx) {
        match (self, other) {
            (Expr::Lit(i), ast::ExprKind::Lit(j)) => 
                i.is_match(cx, j),
            (Expr::Block_(i), ast::ExprKind::Block(j, _label)) => 
                i.is_match(cx, j),
            (Expr::Array(i), ast::ExprKind::Array(j)) => 
                i.is_match(cx, j),
            (Expr::If(i_check, i_then, i_else), ast::ExprKind::If(j_check, j_then, j_else)) => {
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
            (Expr::IfLet(i_block, i_else), ast::ExprKind::IfLet(_pattern, _check, j_block, j_else)) => {
                // TODO: also check pattern and expr
                let cx_orig = cx.clone();
                let (r_b, cx) = i_block.is_match(cx, j_block);
                let (r_e, cx) = i_else.is_match(cx, j_else);
                if r_b && r_e {
                    (true, cx)
                } else {
                    *cx = cx_orig;
                    (false, cx)
                }
            },
            // no match otherwise
            (Expr::Lit(_), _) |
            (Expr::Block_(_), _) |
            (Expr::Array(_), _) |
            (Expr::If(_, _, _), _) |
            (Expr::IfLet(_, _), _) => (false, cx)
        }
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, ast::Expr> for Expr<'cx, 'o, Cx, Ast> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o ast::Expr) -> (bool, &'cx mut Cx) {
        self.is_match(cx, &other.node)
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, ast::StmtKind> for Stmt<'cx, 'o, Cx, Ast> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o ast::StmtKind) -> (bool, &'cx mut Cx) {
        match (self, other) {
            (Stmt::Expr(i), ast::StmtKind::Expr(j)) |
            (Stmt::Semi(i), ast::StmtKind::Semi(j)) => i.is_match(cx, j),
            // no match otherwise
            (Stmt::Expr(_), _) |
            (Stmt::Semi(_), _) => (false, cx)
        }
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

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, ast::LitIntType> for LitIntType<'cx, 'o, Cx, Ast> {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o ast::LitIntType) -> (bool, &'cx mut Cx) {
        match (self, other) {
            (LitIntType::Signed(i), ast::LitIntType::Signed(j)) => i.is_match(cx, j),
            (LitIntType::Unsigned(i), ast::LitIntType::Unsigned(j)) => i.is_match(cx, j),
            (LitIntType::Unsuffixed, ast::LitIntType::Unsuffixed) => (true, cx),
            // no match otherwise
            (LitIntType::Signed(_), _) |
            (LitIntType::Unsigned(_), _) |
            (LitIntType::Unsuffixed, _) => (false, cx)
        }
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, ast::IntTy> for IntTy {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o ast::IntTy) -> (bool, &'cx mut Cx) {
        match (self, other) {
            (IntTy::Isize, ast::IntTy::Isize) |
            (IntTy::I8, ast::IntTy::I8) |
            (IntTy::I16, ast::IntTy::I16) |
            (IntTy::I32, ast::IntTy::I32) |
            (IntTy::I64, ast::IntTy::I64) |
            (IntTy::I128, ast::IntTy::I128) => (true, cx),
            // no match otherwise
            (IntTy::Isize, _) |
            (IntTy::I8, _) |
            (IntTy::I16, _) |
            (IntTy::I32, _) |
            (IntTy::I64, _) |
            (IntTy::I128, _) => (false, cx)
        }
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, ast::UintTy> for UintTy {
    fn is_match(&self, cx: &'cx mut Cx, other: &'o ast::UintTy) -> (bool, &'cx mut Cx) {
        match (self, other) {
            (UintTy::Usize, ast::UintTy::Usize) |
            (UintTy::U8, ast::UintTy::U8) |
            (UintTy::U16, ast::UintTy::U16) |
            (UintTy::U32, ast::UintTy::U32) |
            (UintTy::U64, ast::UintTy::U64) |
            (UintTy::U128, ast::UintTy::U128) => (true, cx),
            // no match otherwise
            (UintTy::Usize, _) |
            (UintTy::U8, _) |
            (UintTy::U16, _) |
            (UintTy::U32, _) |
            (UintTy::U64, _) |
            (UintTy::U128, _) => (false, cx),
        }
    }
}

impl ReduceSelf for syntax::ast::Stmt {}

impl<T> Reduce for syntax::ptr::P<T> {
    type Target = T;

    fn reduce(&self) -> &Self::Target {
        &*self
    }
}