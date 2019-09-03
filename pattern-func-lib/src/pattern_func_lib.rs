extern crate proc_macro;

use pattern_func::pattern_func;

pattern_func!{
    fn expr_or_semi($expr) {
        Expr($expr) | Semi($expr)
    }
}

pattern_func!{
    fn alternative($a, $b) {
        Alt($a, $b) | Alt($b, $a)
    }
}