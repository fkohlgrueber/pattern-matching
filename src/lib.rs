#![recursion_limit="256"]

#![feature(rustc_private)]

extern crate syntax;

use pattern::pattern;

use pattern_match::matchers::Alt;
use pattern_match::IsMatch;
use pattern_match::ast_match::Ast;

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
    A: MatchAssociations,
{
    var2: Option<&'o A::Lit>,
    var: Option<&'o A::Bool>,
}

fn PAT<'cx, 'o, A>(node: &'o <A as MatchAssociations>::Expr) -> Option<PATStruct<'o, A>> 
where 
    A: MatchAssociations,
    for<'cx2, 'o2> pattern_tree::Expr<'cx2, 'o2, PATStruct<'o2, A>, A>: IsMatch<
        'cx2, 
        'o2, 
        PATStruct<'o2, A>, 
        <A as MatchAssociations>::Expr
    >,
    A::Char: 'o,
    A::Bool: 'o,
    A::Int: 'o,
    A::Stmt: 'o,
    A::BlockType: 'o,
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
    let (r, cx_out) = pattern.is_match(&mut cx, node);
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

    let res: Option<PATStruct<Ast>> = PAT(&real_ast_node);
    dbg!(res);
}

