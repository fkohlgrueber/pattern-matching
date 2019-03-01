#![recursion_limit="256"]

#![feature(rustc_private)]

extern crate syntax;

use pattern::pattern;

pattern!{
    PAT_IF_WITHOUT_ELSE: Expr = 
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
    PAT_IF_2: Expr = 
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
    PAT_SIMPLE: Expr = 
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
        )
);

pattern!(
    PAT: Expr = 
        Lit(Bool(_#var)#var2)#var3
);

pattern!(
    PAT_NESTED: Expr = 
        Array( Array(_*#var_inner)* )
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

    
    use pattern::pattern_match::dummy_ast_match::*;

    let ast_node = DummyExpr::Lit(DummyLit::Bool(false));

    let res = PAT(&real_ast_node);
    dbg!(res);

    let res2 = PAT(&ast_node);
    dbg!(res2);
}


#[test]
fn test_nested() {
    // Nested named fields get flattened
    use pattern::pattern_match::dummy_ast_match::variants::*;
    

    let ast_node = Array(vec!(
        Array(vec!(
            Lit(Int(1)),
            Lit(Int(2))
        )),
        Array(vec!(
            Lit(Int(3)),
            Lit(Int(4))
        ))
    ));

    let res = PAT_NESTED(&ast_node);
    assert_eq!(
        res.map(|x| x.var_inner),
        Some(vec!(
            &Lit(Int(1)),
            &Lit(Int(2)),
            &Lit(Int(3)),
            &Lit(Int(4))
        ))
    );
}

pattern!(
    PAT_NONMATCH_SUBPATTERN: Expr = 
        Array( Lit(Bool(_#var)) Lit(Bool(true)) ) |
        Array( Lit(Bool(false))* )
);

#[test]
fn test_nonmatch_subpattern() {
    // Make sure the result contains only named submatch values for parts of 
    // the pattern that actually matched
    use pattern::pattern_match::dummy_ast_match::variants::*;
    
    let ast_node = Array(vec!(
        Lit(Bool(false)),
        Lit(Bool(false))
    ));

    let res = PAT_NONMATCH_SUBPATTERN(&ast_node);
    assert_eq!(
        res.unwrap().var,
        None
    );
}

pattern!(
    PAT_NONMATCH_SUBPATTERN_NODE: Expr = 
        If(Lit(Bool(_#var)), Block(Expr(Lit(_))), _?) |
        If(_, _, ())
);

#[test]
fn test_nonmatch_subpattern_node() {
    // Make sure the result contains only named submatch values for parts of 
    // the pattern that actually matched
    use pattern::pattern_match::dummy_ast_match::variants::*;
    use pattern::pattern_match::dummy_ast_match::*;
    
    let ast_node = If(
        Box::new(Lit(Bool(false))),
        DummyBlock(vec!(Expr(Array(vec!())))),
        Box::new(None)
    );

    let res = PAT_NONMATCH_SUBPATTERN_NODE(&ast_node);
    assert_eq!(
        res.unwrap().var,
        None
    );
}

pattern!(
    PAT_NONMATCH_SUBPATTERN_REPEAT: Expr = 
        Array( Lit(Bool(true)#var){2} ) |
        _
);

#[test]
fn test_nonmatch_subpattern_repeat() {
    // Make sure the result contains only named submatch values for parts of 
    // the pattern that actually matched
    use pattern::pattern_match::dummy_ast_match::variants::*;
    
    let ast_node = Array(vec!(
        Lit(Bool(true)),
        Lit(Bool(false))
    ));

    let res = PAT_NONMATCH_SUBPATTERN_REPEAT(&ast_node);
    assert_eq!(
        res.unwrap().var,
        None
    );
}