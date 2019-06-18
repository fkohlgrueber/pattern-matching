#[derive(Debug, Clone)]
struct pat_stringTmpStruct<'o, A>
where
    A: pattern::pattern_match::pattern_tree_rust::MatchAssociations<'o>,
{
    root: Option<&'o A::Expr>,
    b: Option<&'o A::Lit>,
    a: Option<&'o A::Symbol>,
}

impl<'o, A> pat_stringTmpStruct<'o, A>
where
    A: pattern::pattern_match::pattern_tree_rust::MatchAssociations<'o>,
{
    fn new() -> pat_stringTmpStruct<'o, A> {
        pat_stringTmpStruct {
            root: None,
            b: None,
            a: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct pat_stringStruct<'o, A>
where
    A: pattern::pattern_match::pattern_tree_rust::MatchAssociations<'o>,
{
    pub root: &'o A::Expr,
    pub b: &'o A::Lit,
    pub a: &'o A::Symbol,
}

impl<'o, A> From<pat_stringTmpStruct<'o, A>> for pat_stringStruct<'o, A>
where
    A: pattern::pattern_match::pattern_tree_rust::MatchAssociations<'o>,
{
    fn from(cx: pat_stringTmpStruct<'o, A>) -> Self {
        pat_stringStruct {
            root: cx.root.unwrap(),
            b: cx.b.unwrap(),
            a: cx.a.unwrap(),
        }
    }
}

fn pat_string<'o, A, P>(node: &'o P) -> Option<pat_stringStruct<'o, A>>
where
    A: pattern::pattern_match::pattern_tree_rust::MatchAssociations<'o, Expr = P>,
    P: std::fmt::Debug + std::clone::Clone,
    for<'cx> pattern::pattern_match::pattern_tree_rust::Expr<'cx, 'o, pat_stringTmpStruct<'o, A>, A>:
        pattern::pattern_match::IsMatch<'cx, 'o, pat_stringTmpStruct<'o, A>, P>,
{
    use pattern::pattern_match::IsMatch;
    let pattern: pattern::matchers::Alt<
        '_,
        '_,
        pattern::pattern_match::pattern_tree_rust::Expr<'_, '_, pat_stringTmpStruct<A>, A>,
        pat_stringTmpStruct<A>,
        A::Expr,
    > = pattern::matchers::Alt::Named(
        Box::new(pattern::matchers::Alt::Elmt(Box::new(
            pattern::pattern_match::pattern_tree_rust::variants::Lit(
                pattern::matchers::Alt::Named(
                    Box::new(pattern::matchers::Alt::Elmt(Box::new(
                        pattern::pattern_match::pattern_tree_rust::variants::Str(
                            pattern::matchers::Alt::Named(
                                Box::new(pattern::matchers::Alt::Elmt(Box::new("abcdef"))),
                                |cx, elmt| {
                                    cx.a = Some(elmt);
                                    cx
                                },
                            ),
                        ),
                    ))),
                    |cx, elmt| {
                        cx.b = Some(elmt);
                        cx
                    },
                ),
            ),
        ))),
        |cx, elmt| {
            cx.root = Some(elmt);
            cx
        },
    );
    let mut cx = pat_stringTmpStruct::new();
    let (r, cx_out) = pattern.is_match(&mut cx, node);
    if r {
        Some(cx.into())
    } else {
        None
    }
}

#[derive(Debug, Clone)]
struct meta_pat_simpleTmpStruct<'o, A>
where
    A: pattern::pattern_match::pattern_tree_rust::MatchAssociations<'o>,
{
    root: Option<&'o A::ParseTree>,
}
impl<'o, A> meta_pat_simpleTmpStruct<'o, A>
where
    A: pattern::pattern_match::pattern_tree_meta::MatchAssociations<'o>,
{
    fn new() -> meta_pat_simpleTmpStruct<'o, A> {
        meta_pat_simpleTmpStruct { root: None }
    }
}
#[derive(Debug, Clone)]
pub struct meta_pat_simpleStruct<'o, A>
where
    A: pattern::pattern_match::pattern_tree_rust::MatchAssociations<'o>,
{
    pub root: &'o A::ParseTree,
}
impl<'o, A> From<meta_pat_simpleTmpStruct<'o, A>> for meta_pat_simpleStruct<'o, A>
where
    A: pattern::pattern_match::pattern_tree_meta::MatchAssociations<'o>,
{
    fn from(cx: meta_pat_simpleTmpStruct<'o, A>) -> Self {
        meta_pat_simpleStruct {
            root: cx.root.unwrap(),
        }
    }
}
fn meta_pat_simple<'o, A, P>(node: &'o P) -> Option<meta_pat_simpleStruct<'o, A>>
where
    A: pattern::pattern_match::pattern_tree_meta::MatchAssociations<'o, ParseTree = P>,
    P: std::fmt::Debug + std::clone::Clone,
    for<'cx> pattern::pattern_match::pattern_tree_meta::ParseTree<
        'cx,
        'o,
        meta_pat_simpleTmpStruct<'o, A>,
        A,
    >: pattern::pattern_match::IsMatch<'cx, 'o, meta_pat_simpleTmpStruct<'o, A>, P>,
{
    use pattern::pattern_match::IsMatch;
    let pattern: pattern::matchers::Alt<
        '_,
        '_,
        pattern::pattern_match::pattern_tree_meta::ParseTree<
            '_,
            '_,
            meta_pat_simpleTmpStruct<A>,
            A,
        >,
        meta_pat_simpleTmpStruct<A>,
        A::ParseTree,
    > = pattern::matchers::Alt::Named(Box::new(pattern::matchers::Alt::Any), |cx, elmt| {
        cx.root = Some(elmt);
        cx
    });
    let mut cx = meta_pat_simpleTmpStruct::new();
    let (r, cx_out) = pattern.is_match(&mut cx, node);
    if r {
        Some(cx.into())
    } else {
        None
    }
}
