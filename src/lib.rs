#![recursion_limit="256"]

use pattern::pattern;

use lazy_static::lazy_static;

#[derive(Debug, Default)]
pub struct Res<'o, A>
where A: MatchAssociations {
    var: Option<&'o A::Bool>,
    var2: Option<&'o A::Lit>,
    var3: Option<&'o A::Expr>,
}

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

use pattern_tree::matchers::*;

pattern!(
    PAT: Alt<pattern_tree::Expr> = 
        Array( Lit(Bool(_#var|_)#var2)*#var3 )
);


#[test]
fn test() {
    //println!("THIS IS THE PATTERN: {:?}", *PAT);

    let ast_node = AstExpr::Lit(AstLit::Bool(false));

    PAT(&ast_node);
}

