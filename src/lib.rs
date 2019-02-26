#![recursion_limit="256"]

#![feature(rustc_private)]

extern crate syntax;

use pattern::pattern;

use pattern_match::matchers::Alt;
use pattern_match::IsMatch;

use pattern_match::pattern_tree;
use pattern_match::MatchAssociations;

/*
pattern!(
    PAT: Alt<pattern_tree::Expr> = 
        //Array( Lit(Bool(_#var|_)#var2)*#var3 )
        Lit(Bool(_#var|_)#var2)
);*/


#[derive(Debug)]
pub struct PATStruct<'o, A>
where
    A: MatchAssociations<'o>,
{
    var2: Option<&'o A::Lit>,
    var: Option<&'o A::Bool>,
}

use std::fmt::Debug;

fn PAT<'o, A, P>(node: &'o P) -> Option<PATStruct<'o, A>> 
where 
    A: MatchAssociations<'o, Expr=P>,
    P: Debug,
    for<'cx> pattern_tree::Expr<'cx, 'o, PATStruct<'o, A>, A>: IsMatch<
        'cx, 
        'o, 
        PATStruct<'o, A>, 
        P
    >,
{
    let pattern: pattern_tree::Expr<'_, '_, PATStruct<A>, A>
    = pattern_tree::variants::Lit(
        pattern_match::matchers::Alt::Named(
            Box::new(pattern_match::matchers::Alt::Elmt(Box::new(
                pattern_tree::variants::Bool(pattern_match::matchers::Alt::Alt(
                    Box::new(pattern_match::matchers::Alt::Named(
                        Box::new(pattern_match::matchers::Alt::Any),
                        |cx, elmt| {
                            cx.var = Some(elmt);
                            cx
                        },
                    )),
                    Box::new(pattern_match::matchers::Alt::Any),
                )),
            ))),
            |cx, elmt| {
                cx.var2 = Some(elmt);
                cx
            },
        ),
    );
    let mut cx = PATStruct {
        var2: None,
        var: None,
    };
    let (r, _cx_out) = pattern.is_match(&mut cx, node);
    if r {
        Some(cx)
    } else {
        None
    }
}





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

    
    //let ast_node = AstExpr::Lit(AstLit::Bool(false));

    let res = PAT(&real_ast_node);
    dbg!(res);
}

