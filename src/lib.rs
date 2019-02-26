#![recursion_limit="256"]

#![feature(rustc_private)]

extern crate syntax;

use pattern::pattern;
/*
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

#[derive(Debug)]
pub enum AstBlock {
    Block(AstStmt),
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
    type BlockType = AstBlock;
}
*/
use pattern_tree::matchers::Alt;
use pattern_match::IsMatch;
use pattern_match::ast_match::Ast;

pattern!(
    PAT: Alt<pattern_tree::Expr> = 
        //Array( Lit(Bool(_#var|_)#var2)*#var3 )
        Lit(Bool(_#var|_)#var2)
);


#[test]
fn test() {
    use syntax::ast;
    let real_ast_node = ast::Expr {
        attrs: syntax::ThinVec::new(),
        span: syntax::source_map::Span::default(),
        id: ast::NodeId::from_usize(0),
        node: ast::ExprKind::Lit(
            syntax::source_map::Spanned {
                span: syntax::source_map::Span::default(),
                node: ast::LitKind::Bool(false),
            }
        )
    };
    
    //println!("THIS IS THE PATTERN: {:?}", *PAT);

    
    //let ast_node = AstExpr::Lit(AstLit::Bool(false));

    dbg!(PAT(&real_ast_node));
}

