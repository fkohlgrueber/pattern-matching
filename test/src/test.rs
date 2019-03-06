#![recursion_limit="256"]

#![feature(rustc_private)]

extern crate syntax;

use pattern::pattern;

pattern!(
    pat: Expr = 
        Lit(Bool(_#var)#var2)#var3
);

pattern!(
    pat_nested: Expr = 
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

    let res = pat(&real_ast_node);
    dbg!(res);

    let res2 = pat(&ast_node);
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

    let res = pat_nested(&ast_node);
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
    pat_nonmatch_subpattern: Expr = 
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

    let res = pat_nonmatch_subpattern(&ast_node);
    assert_eq!(
        res.unwrap().var,
        None
    );
}

pattern!(
    pat_nonmatch_subpattern_node: Expr = 
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

    let res = pat_nonmatch_subpattern_node(&ast_node);
    assert_eq!(
        res.unwrap().var,
        None
    );
}

pattern!(
    pat_nonmatch_subpattern_repeat: Expr = 
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

    let res = pat_nonmatch_subpattern_repeat(&ast_node);
    assert_eq!(
        res.unwrap().var,
        None
    );
}