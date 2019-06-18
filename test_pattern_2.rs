/*
pattern!{
    my_pattern: Expr = 
        Array( 
            Lit(_)* #literals 
            Lit(Bool(_#boolean))
        )
}
*/


#[derive(Debug, Clone)]
struct my_patternTmpStruct<'o, A>
where A: pattern_tree_rust::MatchAssociations<'o> {
    boolean: Option<&'o bool>,
    root: Option<&'o A::Expr>,
    literals: Vec<&'o A::Expr>
}

impl<'o, A> my_patternTmpStruct<'o, A>
where A: pattern_tree_rust::MatchAssociations<'o> {
    fn new() -> my_patternTmpStruct<'o, A> {
        my_patternTmpStruct {
            boolean: None,
            root: None,
            literals: vec![]
        }
    }
}

#[derive(Debug, Clone)]
pub struct my_patternStruct<'o, A>
where A: pattern_tree_rust::MatchAssociations<'o> {
    pub boolean: &'o bool,
    pub root: &'o A::Expr,
    pub literals: Vec<&'o A::Expr>
}

impl<'o, A> From<my_patternTmpStruct<'o, A>> for my_patternStruct<'o, A>
where A: pattern_tree_rust::MatchAssociations<'o> {
    fn from(cx: my_patternTmpStruct<'o, A>) -> Self {
        my_patternStruct {
            boolean: cx.boolean.unwrap(),
            root: cx.root.unwrap(),
            literals: cx.literals
        }
    }
}

fn my_pattern<'o, A, P>(node: &'o P) -> Option<my_patternStruct<'o, A>>
where
    A: pattern_tree_rust::MatchAssociations<'o, Expr = P>,
    P: std::fmt::Debug + std::clone::Clone,
    for<'cx> pattern_tree_rust::Expr<'cx, 'o, my_patternTmpStruct<'o, A>, A>:
        IsMatch<'cx, 'o, my_patternTmpStruct<'o, A>, P>,
{
    // initialize the pattern
    let pattern: Alt<
        '_,
        '_,
        pattern_tree_rust::Expr<'_, '_, my_patternTmpStruct<A>, A>,
        my_patternTmpStruct<A>,
        A::Expr,
    > = Alt::Named(
        Box::new(Alt::Elmt(Box::new(
            pattern_tree_rust::variants::Array(
                Seq::Seq(
                    Box::new(Seq::Named(
                        Box::new(Seq::Repeat(
                            Box::new(Seq::Elmt(Box::new(
                                pattern_tree_rust::variants::Lit(
                                    Alt::Any,
                                ),
                            ))),
                            RepeatRange {
                                start: 0,
                                end: None,
                            },
                        )),
                        |cx, elmt| {
                            cx.literals.push(elmt);
                            cx
                        },
                    )),
                    Box::new(Seq::Elmt(Box::new(
                        pattern_tree_rust::variants::Lit(
                            Alt::Elmt(Box::new(
                                pattern_tree_rust::variants::Bool(
                                    Alt::Named(
                                        Box::new(Alt::Any),
                                        |cx, elmt| {
                                            cx.boolean = Some(elmt);
                                            cx
                                        },
                                    ),
                                ),
                            )),
                        ),
                    ))),
                ),
            ),
        ))),
        |cx, elmt| {
            cx.root = Some(elmt);
            cx
        },
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
