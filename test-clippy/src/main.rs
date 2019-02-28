#![feature(box_syntax)]
#![feature(rustc_private)]

extern crate rustc;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate syntax;

use rustc::{declare_lint, lint_array};
use rustc::lint::*;
use rustc_driver::driver;

use pattern::pattern;

declare_lint! {
    pub COLLAPSIBLE_IF,
    Forbid,
    "`if`s that can be collapsed (e.g. `if x { if y { ... } }` and `else { if x { ... } }`)"
}

pub struct CollapsibleIf;

impl LintPass for CollapsibleIf {
    fn get_lints(&self) -> LintArray {
        lint_array!(COLLAPSIBLE_IF)
    }
}

pattern!{
    PAT_IF_WITHOUT_ELSE: Expr = 
        If(
            _#check,
            Block(
                Expr( If(_#check_inner, _#content, ())#inner )
                | Semi( If(_#check_inner, _#content, ())#inner ) 
            )#then, 
            ()
        )
}

pattern!{
    PAT_IF_2: Expr = 
        If(
            _, 
            _, 
            Block_(
                Block(
                    Expr((If(_, _, _?) | IfLet(_, _?))#else_) | 
                    Semi((If(_, _, _?) | IfLet(_, _?))#else_)
                )
            )#block
        ) |
        IfLet(
            _, 
            Block_(
                Block(
                    Expr((If(_, _, _?) | IfLet(_, _?))#else_) | 
                    Semi((If(_, _, _?) | IfLet(_, _?))#else_)
                )
            )#block
        )
}

impl EarlyLintPass for CollapsibleIf {
    fn check_expr(&mut self, cx: &EarlyContext, expr: &syntax::ast::Expr) {
        
        if PAT_IF_WITHOUT_ELSE(expr).is_some() {
            cx.span_lint(
                SIMPLE_PATTERN,
                expr.span,
                "this if statement can be collapsed",
            );
        }
        match PAT_IF_2(expr) {
            Some(res) => {
                cx.span_lint(
                    SIMPLE_PATTERN,
                    res.block.span,
                    "this `else { if .. }` block can be collapsed",
                );
            },
            _ => ()
        };
    }
}


declare_lint! {
    pub SIMPLE_PATTERN,
    Forbid,
    "simple pattern lint"
}

pub struct SimplePattern;

impl LintPass for SimplePattern {
    fn get_lints(&self) -> LintArray {
        lint_array!(SIMPLE_PATTERN)
    }
}

pattern!(
    PAT_SIMPLE: Expr = 
        Lit(Bool(false)) |
        Array(
            Lit(Char('a')) * 
            Lit(Char('b')) {1,3} 
            Lit(Char('c'))
        ) |
        If(
            Lit(Bool(true)), 
            Block(Expr(Lit(Int(_)))* Semi(Lit(Bool(_)))*),
            _?
        )#var
);

impl EarlyLintPass for SimplePattern {
    fn check_expr(&mut self, cx: &EarlyContext, expr: &syntax::ast::Expr) {
        
        if PAT_SIMPLE(expr).is_some() {
            cx.span_lint(
                SIMPLE_PATTERN,
                expr.span,
                "This is a match for a simple pattern. Well Done!",
            );
        }
        
    }
}


pub fn main() {
    let args: Vec<_> = std::env::args().collect();
    rustc_driver::run(move || {
        let mut compiler = driver::CompileController::basic();
        compiler.after_parse.callback = Box::new(move |state| {
            let mut ls = state.session.lint_store.borrow_mut();
            ls.register_early_pass(None, false, box SimplePattern);
            ls.register_early_pass(None, false, box CollapsibleIf);
        });
        rustc_driver::run_compiler(&args, Box::new(compiler), None, None)
    });
}
