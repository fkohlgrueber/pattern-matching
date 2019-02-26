#![recursion_limit="256"]

#![feature(rustc_private)]

extern crate syntax;

use pattern::pattern;

use pattern_match::matchers::Alt;
use pattern_match::IsMatch;
use pattern_match::ast_match::Ast;

use pattern_match::pattern_tree;

/*
pattern!(
    PAT: Alt<pattern_tree::Expr> = 
        //Array( Lit(Bool(_#var|_)#var2)*#var3 )
        Lit(Bool(_#var|_)#var2)
);*/


#[derive(Debug)]
pub struct PATStruct<'cx, 'o, A>
where
    A: pattern_match::MatchAssociations<'cx, 'o, Self>,
{
    var2: Option<&'o A::Lit>,
    var: Option<&'o A::Bool>,
}

fn PAT<'cx, 'o>(node: &'o <Ast as pattern_match::MatchAssociations<'cx, 'o, PATStruct<'cx, 'o, Ast>>>::Expr) -> Option<PATStruct<'cx, 'o, Ast>> {
    let pattern: Alt<
        '_, 
        '_, 
        pattern_tree::Expr<'_, '_, PATStruct<Ast>, Ast>, 
        PATStruct<Ast>, 
        <Ast as pattern_match::MatchAssociations<'cx, 'o, PATStruct<'cx, 'o, Ast>>>::Expr, 
    > 
    = pattern_match::matchers::Alt::Elmt(Box::new(pattern_tree::variants::Lit(
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
    )));
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

    dbg!(PAT(&real_ast_node));
}

