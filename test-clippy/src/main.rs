#![feature(box_syntax)]
#![feature(rustc_private)]

extern crate rustc;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_interface;
extern crate syntax;

use rustc::{declare_lint, lint_array};
use rustc::lint::*;
use rustc_interface::interface;

use pattern::pattern;
use pattern::meta_pattern;
use pattern::pattern_mini;
use pattern_parse::parse_pattern_str;

mod utils;

use syntax::ast;

use crate::utils::snippet_block;

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

use pattern_func_lib::expr_or_semi;

pattern!{
    pat_if_without_else: Expr = 
        If(
            _#check,
            Block(
                expr_or_semi( If(_#check_inner, _#content, ())#inner )
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
                    expr_or_semi( If(_, _, _?)#else_ )
                )#block_inner
            )#block
        )
}

impl EarlyLintPass for CollapsibleIf {
    fn check_expr(&mut self, cx: &EarlyContext, expr: &syntax::ast::Expr) {
        
        if expr.span.from_expansion() {
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
            if !block_starts_with_comment(cx, res.block_inner) && !res.else_.span.from_expansion() {
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

declare_lint! {
    pub STRING_PATTERN,
    Forbid,
    "simple pattern lint"
}
pub struct StringPattern;

impl LintPass for StringPattern {
    fn get_lints(&self) -> LintArray {
        lint_array!(STRING_PATTERN)
    }

    fn name(&self) -> &'static str {
        "StringPattern"
    }
}

use pattern_func_lib::alternative;

pattern!{
    pat_string: Expr = 
        Lit(Str("abcdef"#a)#b)
}

impl EarlyLintPass for StringPattern {
    fn check_expr(&mut self, cx: &EarlyContext, expr: &syntax::ast::Expr) {
        
        match pat_string(expr) {
            Some(_res) => {
                //let inner = res.a;
                //let outer = res.b;
                cx.span_lint(
                    STRING_PATTERN,
                    expr.span,
                    "This is a match for the string pattern. Well Done too!",
                );
            },
            None => ()
        }
        
    }
}

declare_lint! {
    pub MINI_PATTERN,
    Forbid,
    "mini pattern lint"
}
pub struct MiniPattern;

impl LintPass for MiniPattern {
    fn get_lints(&self) -> LintArray {
        lint_array!(MINI_PATTERN)
    }

    fn name(&self) -> &'static str {
        "MiniPattern"
    }
}

pattern_mini!{
    pat_mini: Expr = 
        If(
            Lit(Bool(true)),
            Block(
                Semi(Lit(Bool(true)))? 
                expr_or_semi(Lit(Char('x')))
            ), 
            Block_(Block(Expr(Lit(Char('y')))))
        )
}

impl EarlyLintPass for MiniPattern {
    fn check_expr(&mut self, cx: &EarlyContext, expr: &syntax::ast::Expr) {
        
        match pat_mini(expr) {
            Some(_res) => {
                //let inner = res.a;
                //let outer = res.b;
                cx.span_lint(
                    MINI_PATTERN,
                    expr.span,
                    "This is a match for the mini pattern. Well Done too!",
                );
            },
            None => ()
        }
        
    }
}

declare_lint! {
    pub PRE_LINT,
    Forbid,
    "pre expansion lint"
}
pub struct PreLint;

impl LintPass for PreLint {
    fn get_lints(&self) -> LintArray {
        lint_array!(PRE_LINT)
    }

    fn name(&self) -> &'static str {
        "PreLint"
    }
}

meta_pattern!{
    pre_lint: ParseTree = 
        alternative(_, Any_)
        // Alt(_, Any_) | Alt(Any_, _)
}

impl EarlyLintPass for PreLint {
    fn check_mac(&mut self, cx: &EarlyContext, mac: &syntax::ast::Mac) {
        if mac.path.to_string() != "pattern" {
            return;
        }
        match parse_pattern_str(&mac.tts.to_string()) {
            Ok(pattern) => match pre_lint(&pattern.node) {
                Some(_res) => {
                    //let inner = res.a;
                    //let outer = res.b;
                    cx.span_lint(
                        PRE_LINT,
                        mac.span,
                        "One side of the pattern has no effect because _ matches averything.",
                    );
                },
                None => ()
            },
            _ => ()
        }
    }
}

struct ClippyCallbacks;

impl rustc_driver::Callbacks for ClippyCallbacks {
    fn after_parsing(&mut self, compiler: &interface::Compiler) -> rustc_driver::Compilation {
        let sess = compiler.session();
        
        let mut ls = sess.lint_store.borrow_mut();
        ls.register_early_pass(None, false, false, box SimplePattern);
        ls.register_early_pass(None, false, false, box StringPattern);
        ls.register_early_pass(None, false, false, box CollapsibleIf);
        ls.register_early_pass(None, false, false, box MiniPattern);
        ls.register_pre_expansion_pass(None, false, false, box PreLint);

        // Continue execution
        rustc_driver::Compilation::Continue
    }
}
#[allow(unused_must_use)]
pub fn main() {
    let args: Vec<_> = std::env::args().collect();
    rustc_driver::report_ices_to_stderr_if_any(move || {
        rustc_driver::run_compiler(&args, &mut ClippyCallbacks, None, None)
    });
}
