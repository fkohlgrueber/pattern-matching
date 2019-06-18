

struct my_patternTmpStruct<'o, A>
where A: pattern_tree_rust::MatchAssociations<'o> {
    literals: Option<&'o A::Lit>,
    boolean: Option<&'o bool>,
    root: Option<&'o A::Expr>,
}

impl<'o, A> my_patternTmpStruct<'o, A>
where A: pattern_tree_rust::MatchAssociations<'o> {
    fn new() -> my_patternTmpStruct<'o, A> {
        my_patternTmpStruct {
            literals: None,
            boolean: None,
            root: None,
        }
    }
}


pub struct my_patternStruct<'o, A>
where A: pattern_tree_rust::MatchAssociations<'o> {
    pub literals: &'o A::Lit,
    pub boolean: &'o bool,
    pub root: &'o A::Expr,
}
impl<'o, A> From<my_patternTmpStruct<'o, A>> for my_patternStruct<'o, A>
where
    A: pattern::pattern_match::pattern_tree_rust::MatchAssociations<'o>,
{
    fn from(cx: my_patternTmpStruct<'o, A>) -> Self {
        my_patternStruct {
            literals: cx.literals.unwrap(),
            boolean: cx.boolean.unwrap(),
            root: cx.root.unwrap(),
        }
    }
}
fn my_pattern<'o, A, P>(node: &'o P) -> Option<my_patternStruct<'o, A>>
where
    A: pattern::pattern_match::pattern_tree_rust::MatchAssociations<'o, Expr = P>,
    P: std::fmt::Debug + std::clone::Clone,
    for<'cx> pattern::pattern_match::pattern_tree_rust::Expr<'cx, 'o, my_patternTmpStruct<'o, A>, A>:
        pattern::pattern_match::IsMatch<'cx, 'o, my_patternTmpStruct<'o, A>, P>,
{
    use pattern::pattern_match::IsMatch;
    let pattern: pattern::matchers::Alt<
        '_,
        '_,
        pattern::pattern_match::pattern_tree_rust::Expr<'_, '_, my_patternTmpStruct<A>, A>,
        my_patternTmpStruct<A>,
        A::Expr,
    > = pattern::matchers::Alt::Named(
        Box::new(pattern::matchers::Alt::Elmt(Box::new(
            pattern::pattern_match::pattern_tree_rust::variants::Array(
                pattern::matchers::Seq::Seq(
                    Box::new(pattern::matchers::Seq::Repeat(
                        Box::new(pattern::matchers::Seq::Elmt(Box::new(
                            pattern::pattern_match::pattern_tree_rust::variants::Lit(
                                pattern::matchers::Alt::Named(
                                    Box::new(pattern::matchers::Alt::Any),
                                    |cx, elmt| {
                                        cx.literals = Some(elmt);
                                        cx
                                    },
                                ),
                            ),
                        ))),
                        pattern::matchers::RepeatRange {
                            start: 0,
                            end: None,
                        },
                    )),
                    Box::new(pattern::matchers::Seq::Elmt(Box::new(
                        pattern::pattern_match::pattern_tree_rust::variants::Lit(
                            pattern::matchers::Alt::Elmt(Box::new(
                                pattern::pattern_match::pattern_tree_rust::variants::Bool(
                                    pattern::matchers::Alt::Named(
                                        Box::new(pattern::matchers::Alt::Any),
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
    let mut cx = my_patternTmpStruct::new();
    let (r, cx_out) = pattern.is_match(&mut cx, node);
    if r {
        Some(cx.into())
    } else {
        None
    }
}
