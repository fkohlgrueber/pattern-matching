/*
pattern!{
    my_pattern: Expr =
        _
}
*/

#[derive(Debug, Clone)]
struct my_patternTmpStruct<'o, A>
where A: pattern_tree_rust::MatchAssociations<'o> {
    root: Option<&'o A::Expr>
}

impl<'o, A> my_patternTmpStruct<'o, A>
where A: pattern_tree_rust::MatchAssociations<'o> {
    fn new() -> my_patternTmpStruct<'o, A> {
        my_patternTmpStruct { root: None }
    }
}

#[derive(Debug, Clone)]
pub struct my_patternStruct<'o, A>
where A: pattern_tree_rust::MatchAssociations<'o> {
    pub root: &'o A::Expr
}

impl<'o, A> From<my_patternTmpStruct<'o, A>> for my_patternStruct<'o, A>
where A: pattern_tree_rust::MatchAssociations<'o> {
    fn from(cx: my_patternTmpStruct<'o, A>) -> Self {
        my_patternStruct {
            root: cx.root.unwrap()
        }
    }
}

fn my_pattern<'o, A, P>(node: &'o P) -> Option<my_patternStruct<'o, A>>
where
    A: pattern_tree_rust::MatchAssociations<'o, Expr = P>,
    P: std::fmt::Debug + std::clone::Clone,
    for<'cx> pattern_tree_rust::Expr<'cx, 'o, my_patternTmpStruct<'o, A>, A>:
        IsMatch<'cx, 'o, my_patternTmpStruct<'o, A>, P>
{
    // initialize the pattern
    let pattern: pattern::matchers::Alt<
        '_,
        '_,
        pattern_tree_rust::Expr<'_, '_, my_patternTmpStruct<A>, A>,
        my_patternTmpStruct<A>,
        A::Expr
    > = pattern::matchers::Alt::Named(
        Box::new(pattern::matchers::Alt::Any), 
        |cx, elmt| {
            cx.root = Some(elmt);
            cx
        }
    );
    // initialize the (temporary) result struct
    let mut cx = my_patternTmpStruct::new();

    // match input node against pattern
    let (r, cx_out) = pattern.is_match(&mut cx, node);
    
    if r {
        // convert cx to final result struct and return
        Some(cx.into())
    } else {
        None
    }
}
