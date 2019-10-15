#[derive(Debug)]
struct patTmpStruct < 'o, A > where A : pattern :: pattern_match :: pattern_tree_rust :: MatchAssociations < 'o >
{
    var3: Option<&'o A::Expr>, 
    var: Option<&'o bool>, 
    root: Option<&'o A::Expr>,
    var2: Option<&'o A::Lit>,
} 
impl < 'o, A > patTmpStruct < 'o, A > where A : pattern :: pattern_match :: pattern_tree_rust :: MatchAssociations < 'o >
{
    fn new () -> patTmpStruct < 'o, A >
    { patTmpStruct { var3 : None, var : None, root : None, var2 : None } }
} 

impl<'o, A> Clone for patTmpStruct<'o, A> where A: pattern::pattern_match::pattern_tree_rust::MatchAssociations<'o>
{
    fn clone(&self) -> Self {
        Self {
            var3: self.var3.clone(),
            var: self.var.clone(),
            root: self.root.clone(),
            var2: self.var2.clone(),
        }
    }
}

# [derive (Debug)] pub struct patStruct < 'o, A > where A : pattern
:: pattern_match :: pattern_tree_rust :: MatchAssociations < 'o >
{
    pub var3 : & 'o A :: Expr, pub var : & 'o bool, pub root : & 'o A :: Expr,
    pub var2 : & 'o A :: Lit
} impl < 'o, A > From < patTmpStruct < 'o, A >> for patStruct < 'o, A > where
A : pattern :: pattern_match :: pattern_tree_rust :: MatchAssociations < 'o >
{
    fn from (cx : patTmpStruct < 'o, A >) -> Self
    {
        patStruct
        {
            var3 : cx . var3 . unwrap (), var : cx . var . unwrap (), root :
            cx . root . unwrap (), var2 : cx . var2 . unwrap ()
        }
    }
} 

fn pat<'o, A, P>(node: &'o P) -> Option<patStruct<'o, A>> 
where 
    A: pattern::pattern_match::pattern_tree_rust::MatchAssociations<'o, Expr=P>, 
    P: std::fmt::Debug, 
    for<'cx> pattern::pattern_match::pattern_tree_rust::Expr<'cx, 'o, patTmpStruct<'o, A>, A>: 
        pattern::pattern_match::IsMatch<'cx, 'o, patTmpStruct<'o, A>, P>,
{
    use pattern::pattern_match::IsMatch;
    let pattern: pattern::matchers::Alt<'_, '_, pattern::pattern_match::pattern_tree_rust::Expr<'_, '_, patTmpStruct<A>, A>, patTmpStruct<A>, A::Expr> = pattern::matchers::Alt::Any;

    let mut cx = patTmpStruct::new();
    let (r, cx_out) = pattern.is_match(& mut cx, node);
    if r { 
        Some(cx.into()) 
    } else { 
        None
    }
}

