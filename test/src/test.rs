#![recursion_limit="256"]

#![feature(rustc_private)]

extern crate syntax;

use pattern::pattern;
use pattern::meta_pattern;

use pattern_func_lib::expr_or_semi;

pattern!{
    my_pattern: Stmt =
        expr_or_semi(Lit(_))
}

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
            Lit(Int(1, Unsuffixed)),
            Lit(Int(2, Unsuffixed))
        )),
        Array(vec!(
            Lit(Int(3, Unsuffixed)),
            Lit(Int(4, Unsuffixed))
        ))
    ));

    let res = pat_nested(&ast_node);
    assert_eq!(
        res.map(|x| x.var_inner),
        Some(vec!(
            &Lit(Int(1, Unsuffixed)),
            &Lit(Int(2, Unsuffixed)),
            &Lit(Int(3, Unsuffixed)),
            &Lit(Int(4, Unsuffixed))
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

pattern!{
    pat_int_suffix: Expr = 
        Lit( Int(_, Unsuffixed | Signed(I32)))
}

#[test]
fn test_enum_unit_variant() {
    use pattern::pattern_match::dummy_ast_match::variants::*;
    
    let ast_node = Lit( Int(123, Unsuffixed));
    let ast_node_2 = Lit( Int(123, Signed(I32)));
    let ast_node_3 = Lit( Int(123, Signed(I64)));

    assert!( pat_int_suffix(&ast_node).is_some() );
    assert!( pat_int_suffix(&ast_node_2).is_some() );
    assert!( pat_int_suffix(&ast_node_3).is_none() );
}


meta_pattern!{
    meta_pat_simple: ParseTree = 
        Node(_, _*)
}


#[test]
fn test_meta_pattern() {
    use pattern_parse::parse_pattern_str;
    let pattern = parse_pattern_str("test_pat: Expr = Lit(Bool(true))").unwrap();

    assert!(meta_pat_simple(&pattern.node).is_some())
}

meta_pattern!{
    meta_pat_complicated_range: ParseTree = 
        Repeat_(_, Range(0, 1))
}

#[test]
fn test_meta_pattern_complicated_range() {
    use pattern_parse::parse_pattern_str;
    let pattern = parse_pattern_str("test_pat: Expr = Lit(Bool(true)){0, 1}").unwrap();

    assert!(meta_pat_complicated_range(&pattern.node).is_some());

    let pattern = parse_pattern_str("test_pat: Expr = Lit(Bool(true)){0, 2}").unwrap();
    assert!(meta_pat_complicated_range(&pattern.node).is_none());
}

meta_pattern!{
    meta_pat_any_or: ParseTree = 
        Alt(Any_, _) | Alt(_, Any_)
}

#[test]
fn test_meta_pattern_any_or() {
    use pattern_parse::parse_pattern_str;
    let pattern1 = parse_pattern_str("test_pat: Expr = _|Lit(_)").unwrap();
    let pattern2 = parse_pattern_str("test_pat: Expr = Lit(_)|_").unwrap();

    assert!(meta_pat_any_or(&pattern1.node).is_some());
    assert!(meta_pat_any_or(&pattern2.node).is_some());

    let pattern = parse_pattern_str("test_pat: Expr = ()|Lit(_)").unwrap();
    assert!(meta_pat_any_or(&pattern.node).is_none());
}
