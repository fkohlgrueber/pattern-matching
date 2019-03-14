extern crate proc_macro;

use pattern_func::pattern_func;

pattern_func!{
    fn expr_or_semi($expr) {
        Expr($expr) | Semi($expr)
    }
}


pattern_func!{
    fn if_or_if_let($then, $else_) {
        If(_, $then, $else_) | IfLet($then, $else_)
    }
}
