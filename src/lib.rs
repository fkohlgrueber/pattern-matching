#![recursion_limit="256"]

#![feature(rustc_private)]

extern crate syntax;

use pattern::pattern;

use pattern_match::matchers::Alt;
use pattern_match::IsMatch;

use pattern_match::pattern_tree;
use pattern_match::MatchAssociations;


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

    let res = PAT(&real_ast_node);
    dbg!(res);
}

