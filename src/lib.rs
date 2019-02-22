#![recursion_limit="256"]

use pattern::pattern;

use lazy_static::lazy_static;

#[derive(Debug)]
struct Res {}

#[derive(Debug)]
pub struct Ast {}

#[derive(Debug)]
pub enum AstExpr {
    Lit(AstLit),
    Array(Vec<AstExpr>),
    Block(Vec<AstStmt>),
    If(Box<AstExpr>, Vec<AstStmt>, Box<Option<AstExpr>>)
}

#[derive(Debug)]
pub enum AstLit {
    Char(char),
    Bool(bool),
    Int(u128),
}

#[derive(Debug)]
pub enum AstStmt {
    Expr(AstExpr),
    Semi(AstExpr),
}


// IMPL
use pattern_tree::MatchAssociations;

impl MatchAssociations for Ast {
    type Expr = AstExpr;
    type Lit = AstLit;
    type Bool = bool;
    type Char = char;
    type Int = u128;
    type Stmt = AstStmt;
}

pattern!(
    PAT: pattern_tree::Expr<'_, '_, Res, Ast> = 
        Array(
            Array(_) | Lit(Bool(_) | Int(_) | Char(_)) |
            Array( Array(()) Lit(Bool(_)){1, 2} )
        )
);


#[test]
fn test() {
    //println!("THIS IS THE PATTERN: {:?}", *PAT);
    PAT();
}

