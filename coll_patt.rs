pattern!{pattern_if_without_else:Expr=If(_#check,Block(expr_or_semi(If(_#check_inner,_#content,())#inner))#then,())}pattern!{pat_if_else:Expr=if_or_if_let(_,Block_(Block(expr_or_semi(if_or_if_let(_,_?)#else_))#block_inner)#block)}impl EarlyLintPass for CollapsibleIf{fn check_expr(&mut self,cx:&EarlyContext<'_>,expr:&ast::Expr){if in_macro(expr.span){return;}match pat_if_without_else(expr){Some(res)=>{if!block_starts_with_comment(cx,res.then)&&expr.span.ctxt()==res.inner.span.ctxt(){}},_=>()}match pat_if_else(expr){Some(res)=>{if!block_starts_with_comment(cx,res.block_inner)&&!in_macro(res.else_.span){}},_=>()};}}