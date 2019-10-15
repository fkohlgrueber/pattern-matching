#![recursion_limit="256"]

#![feature(rustc_private)]

extern crate syntax;

use pattern::pattern;
use pattern::meta_pattern;

pattern!(
    pat: Expr = 
        Lit(Bool(_#var)#var2)#var3
);

pattern!(
    pat_nested: Expr = 
        Array( Array(_*#var_inner)* )
);

pattern!(
    pat_nonmatch_subpattern: Expr = 
        Array( Lit(Bool(_#var)) Lit(Bool(true)) ) |
        Array( Lit(Bool(false))* )
);


pattern!(
    pat_nonmatch_subpattern_node: Expr = 
        If(Lit(Bool(_#var)), Block(Expr(Lit(_))), _?) |
        If(_, _, ())
);


pattern!(
    pat_nonmatch_subpattern_repeat: Expr = 
        Array( Lit(Bool(true)#var){2} ) |
        _
);


pattern!{
    pat_int_suffix: Expr = 
        Lit( Int(_, Unsuffixed | Signed(I32)))
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
