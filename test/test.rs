#![recursion_limit="256"]

#![feature(rustc_private)]

extern crate syntax;

use pattern::pattern;

use pattern_match::IsMatch;

use pattern_match::pattern_tree;

pattern!{
    PAT_IF_WITHOUT_ELSE: Alt<Expr> = 
        If(
            _#check,
            Block(
                Expr( If(_#check_inner, _#content, ())#inner )
                | Semi( If(_#check_inner, _#content, ())#inner ) 
            )#then, 
            ()
        )
}

pattern!{
    PAT_IF_2: Alt<Expr> = 
        If(
            _, 
            _, 
            Block_(
                Block(
                    Expr((If(_, _, _?) | IfLet(_, _?))#else_) | 
                    Semi((If(_, _, _?) | IfLet(_, _?))#else_)
                )
            )#block
        ) |
        IfLet(
            _, 
            Block_(
                Block(
                    Expr((If(_, _, _?) | IfLet(_, _?))#else_) | 
                    Semi((If(_, _, _?) | IfLet(_, _?))#else_)
                )
            )#block
        )
}

pattern!(
    PAT_SIMPLE: Alt<Expr> = 
        Lit(Bool(false#test)) |
        Array(
            Lit(Char('a')) * 
            Lit(Char('b')) {1,3} 
            Lit(Char('c'))
        ) |
        If(
            Lit(Bool(true)), 
            Block(Expr(Lit(Int(_)))* Semi(Lit(Bool(_)))*),
            _?
        )#var
);

pattern!(
    PAT: Alt<Expr> = 
        Lit(Bool(_#var)#var2)#var3
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

    
    use pattern_match::dummy_ast_match::*;

    let ast_node = DummyExpr::Lit(DummyLit::Bool(false));

    let res = PAT(&real_ast_node);
    dbg!(res);

    let res2 = PAT(&ast_node);
    dbg!(res2);
}

