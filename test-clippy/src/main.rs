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

mod utils;

use syntax::ast;

use crate::utils::{snippet_block, in_macro};

fn block_starts_with_comment(cx: &EarlyContext<'_>, expr: &ast::Block) -> bool {
    // We trim all opening braces and whitespaces and then check if the next string is a comment.
    let trimmed_block_text = snippet_block(cx, expr.span, "..")
        .trim_start_matches(|c: char| c.is_whitespace() || c == '{')
        .to_owned();
    trimmed_block_text.starts_with("//") || trimmed_block_text.starts_with("/*")
}

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

    fn name(&self) -> &'static str {
        "CollapsibleIf"
    }
}

pattern!{
    pat_if_without_else: Expr = 
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
    pat_if_else: Expr = 
        If(
            _, 
            _, 
            Block_(
                Block(
                    Expr((If(_, _, _?) | IfLet(_, _?))#else_) | 
                    Semi((If(_, _, _?) | IfLet(_, _?))#else_)
                )#block_inner
            )#block
        ) |
        IfLet(
            _, 
            Block_(
                Block(
                    Expr((If(_, _, _?) | IfLet(_, _?))#else_) | 
                    Semi((If(_, _, _?) | IfLet(_, _?))#else_)
                )#block_inner
            )#block
        )
}

impl EarlyLintPass for CollapsibleIf {
    fn check_expr(&mut self, cx: &EarlyContext, expr: &syntax::ast::Expr) {
        
        if in_macro(expr.span) {
            return;
        }

        if let Some(res) = pat_if_without_else(expr) {
            if !block_starts_with_comment(cx, res.then) && expr.span.ctxt() == res.inner.span.ctxt() {
                cx.span_lint(
                    SIMPLE_PATTERN,
                    expr.span,
                    "this if statement can be collapsed",
                );
            }
        }
        if let Some(res) = pat_if_else(expr) {
            if !block_starts_with_comment(cx, res.block_inner) && !in_macro(res.else_.span){
                cx.span_lint(
                    SIMPLE_PATTERN,
                    res.block.span,
                    "this `else { if .. }` block can be collapsed",
                );
            }
        }
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

    fn name(&self) -> &'static str {
        "SimplePattern"
    }
}

pattern!(
    pat_simple: Expr = 
        Lit(Bool(false)) |
        Array(
            Lit(Char('a')) * 
            Lit(Char('b')) {1,3} 
            Lit(Char('c'))
        ) |
        If(
            Lit(Bool(true)), 
            Block(
                Expr(Lit(Int(_, _)))+ Semi(Lit(Bool(_)))* | 
                Expr(Lit(Int(_, _)))* Semi(Lit(Bool(_)))+
            ),
            _?
        )#var
);

impl EarlyLintPass for SimplePattern {
    fn check_expr(&mut self, cx: &EarlyContext, expr: &syntax::ast::Expr) {
        
        if pat_simple(expr).is_some() {
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
            ls.register_early_pass(None, false, false, box SimplePattern);
            ls.register_early_pass(None, false, false, box CollapsibleIf);
        });
        rustc_driver::run_compiler(&args, Box::new(compiler), None, None)
    });
}
