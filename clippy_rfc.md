- Feature Name: syntax_tree_patterns
- Start Date: (fill me in with today's date, YYYY-MM-DD)
- RFC PR: (leave this empty)
- Rust Issue: (leave this empty)

# Summary
[summary]: #summary

Introduce a domain-specific language (similar to regular expressions) that allows to describe lints using *syntax tree patterns*.


# Motivation
[motivation]: #motivation


Finding parts of a syntax tree (AST, HIR, ...) that have certain properties (e.g. "*an if that has a block as its condition*") is a major task when writing lints. For non-trivial lints, it often requires nested pattern matching of AST / HIR nodes. For example, testing that an expression is a boolean literal requires the following checks:

```
if let ast::ExprKind::Lit(lit) = &expr.node {
    if let ast::LitKind::Bool(_) = &lit.node {
        ...
    }
}
```

Writing this kind of matching code quickly becomes a complex task and the resulting code is often hard to comprehend.

```
// simplified version of the collapsible_if lint
if_chain! {
    if let ast::ExprKind::If(check, then, None) = &expr.node;
    if then.stmts.len() == 1;
    if let ast::StmtKind::Expr(inner) | ast::StmtKind::Semi(inner) = &then.stmts[0].node;
    if let ast::ExprKind::If(check_inner, content, None) = &inner.node;
    then {
        ...
    }
}
```

The code above matches if expressions that contain only another if expression (where both ifs don't have an else branch). While it's easy to explain what the lint does, it's hard to see that from looking at the code sample above.

Following the motivation above, the first goal this RFC is to **simplify writing and reading lints**. 

The second part of the motivation is clippy's dependence on unstable compiler-internal data structures. Clippy lints are currently written against the compiler's AST / HIR which means that even small changes in these data structures might break a lot of lints. The second goal of this RFC is to **make lints independant of the compiler's AST / HIR data structures**.

# Approach

A lot of complexity in writing lints currently seems to come from having to manually implement the matching logic (see code samples above). It's an imparative style that describes *how* to match a syntax tree node instead of specifying *what* should be matched against declaratively. In other areas, it's common to use declarative patterns to describe desired information and let the implementation do the actual matching. A well-known example of this approach are [regular expressions](https://en.wikipedia.org/wiki/Regular_expression). Instead of writing code that detects certain character sequences, one can describe a search pattern using a domain-specific language and search for matches using that pattern. The advantage of using a declarative domain-specific language is that its limited domain (e.g. matching character sequences in the case of regular expressions) allows to express entities in that domain in a very natural and expressive way.

While regular expressions are very useful when searching for patterns in flat character sequences, they cannot easily be applied to hierarchical data structures like syntax trees. This RFC therefore proposes a pattern matching system that is inspired by regular expressions and designed for hierarchical syntax trees.

# Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

This proposal adds a `pattern!` macro that can be used to specify a syntax tree pattern to search for. A simple pattern is shown below:

```
pattern!{
    my_pattern: Expr = 
        Lit(Bool(false))
}
```

This macro call defines a pattern named `my_pattern` that can be matched against an `Expr` syntax tree node. The actual pattern (`Lit(Bool(false))` in this case) defines which syntax trees should match the pattern. This pattern matches expressions that are boolean literals with value `false`.

The pattern can then be used to implement lints in the following way:

```
...

impl EarlyLintPass for MyAwesomeLint {
    fn check_expr(&mut self, cx: &EarlyContext, expr: &syntax::ast::Expr) {
        
        if my_pattern(expr).is_some() {
            cx.span_lint(
                MY_AWESOME_LINT,
                expr.span,
                "This is a match for a simple pattern. Well done!",
            );
        }
        
    }
}
```

The `pattern!` macro call expands to a function `my_pattern` that expects a syntax tree expression as its argument and returns an `Option` that indicates whether the pattern matched.

## Pattern syntax

The following table gives an overview of the pattern syntax:

| Syntax                  | Concept          | Examples                                   |
|-------------------------|------------------|--------------------------------------------|
|`<lit>`                  | Literal          | `'x'`, `false`, `101`                      |
|`<node-name>(<args>)`    | Node             | `Lit(Bool(true))`, `If(_, _, _)`           |
|`_`                      | Any              | `Lit(_)`, `Lit(Char(_))`                   |
|`()`                     | Empty            | `Array( () )`                              |
|`<a> \| <b>`             | Alternation      | `Lit( Char(_) \| Bool(_) )`                |
|`<a> <b>`                | Sequence         | `Tuple( Lit(Bool(_)) Lit(Int(_)) Lit(_) )` |
|`<a>*` <br> `<a>+` <br> `<a>?` <br> `<a>{n}` <br> `<a>{n,m}` <br> `<a>{n,}` | Repetition <br> <br> <br> <br> <br><br> | `Array( _* )`, <br> `Block( Semi(_)+ )`, <br> `If(_, _, Block(_)?)`, <br> `Array( Lit(_){10} )`, <br> `Lit(_){5,10}`, <br> `Lit(Bool(_)){10,}` |
|`<a>#<name>`             | Named submatch   | `Lit(Int(_))#foo` `Lit(Int(_#bar))`        |

## Examples

The following examples demonstate how the pattern syntax can be used:

#### Literal (`<lit>`)

```
pattern!{
    // matches the char 'x'
    my_pattern: Char = 
        'x'
}
```


#### Node (`<node-name>(<args>)`)

```
pattern!{
    // matches if expressions that have `false` as their condition
    my_pattern: Expr = 
        If( Lit(Bool(false)) , _, _?)
}
```

#### Any (`_`)

```
pattern!{
    // matches any expression
    my_pattern: Expr = 
        _
}

pattern!{
    // matches any Literal
    my_pattern: Expr = 
        Lit(_)
}
```

#### Empty (`()`)

```
pattern!{
    // matches if the expression is an empty array
    my_pattern: Expr = 
        Array( () )
}

pattern!{
    // matches if expressions that don't have an else clause
    my_pattern: Expr = 
        If(_, _, ())
}
```


#### Alternations (`a | b`)

```
pattern!{
    // matches if the expression is an array or a literal
    my_pattern: Expr = 
        Lit(_) | Array(_*)
}
```

#### Sequence (`<a> <b>`)

```
pattern!{
    // matches the array [true, false]
    my_pattern: Expr = 
        Array( Lit(Bool(true)) Lit(Bool(false)) )
}
```

#### Repetition (`<a>*`, `<a>+`, `<a>?`, `<a>{n}`, `<a>{n,m}`, `<a>{n,}`)

```
pattern!{
    // matches arrays that contain 5 'x's as their last or second-last elements
    my_pattern: Expr = 
        Array( _* Lit(Char('x')){5} _? )
}

pattern!{
    // matches if expressions that **may or may not** have an else block
    // Attn: `If(_, _, _)` matches only ifs that **have** an else block
    my_pattern: Expr = 
        If(_, _, _?)
}
```

#### Named submatch (`<a>#<name>`)

```
pattern!{
    // matches character literals
    my_pattern: Expr = 
        Lit(Char(_)#foo)
}

pattern!{
    // matches character literals
    my_pattern: Expr = 
        Lit(Char(_#bar))
}

pattern!{
    // matches character literals
    my_pattern: Expr = 
        Lit(Char(_))#baz
}
```

The reason for using named submatches is described in the following section.

## The result type

A lot of lints require checks that go beyond what the pattern syntax described above can express. For example, a lint might want to check whether a node was created as part of a macro expansion or whether there's no comment above a node. Another example would be a lint that wants to match two nodes that have the same value (as needed by lints like `almost_swapped`). Instead of allowing users to write these checks into the pattern directly (which might make patterns hard to read), the proposed solution allows users to assign names to parts of a pattern expression. When matching a pattern against a syntax tree node, the return value will then contain references to all nodes that were matched by these named subpatterns.

For example, given the following pattern

```
pattern!{
    // matches character literals
    my_pattern: Expr = 
        Lit(Char(_#val_inner)#val)#val_outer
}
```

one could get references to the nodes that matched the subpatterns in the following way:

```
...
fn check_expr(expr: &syntax::ast::Expr) {
    if let Some(result) = my_pattern(expr) {
        result.val_inner  // type: &char
        result.val        // type: &syntax::ast::Lit
        result.val_outer  // type: &syntax::ast::Expr
    }
}
```

The types in the `result` struct depend on the pattern. For example, the following patterns

```
pattern!{
    // matches arrays of character literals
    my_pattern_seq: Expr = 
        Array( Lit(_)*#foo )
}
pattern!{
    // matches if expression is a boolean or integer literal
    my_pattern_alt: Expr = 
        Lit( Bool(_#bar) | Int(_) )
}
```

produce the following named submatches:

```
...
fn check_expr(expr: &syntax::ast::Expr) {
    if let Some(result) = my_pattern_seq(expr) {
        result.foo        // type: Vec<&syntax::ast::Expr>
    }
    if let Some(result) = my_pattern_alt(expr) {
        result.bar        // type: Option<&bool>
    }
}
```

Using named subpatterns, users can write lints in two stages. First, a coarse selection of possible matches is produced by the pattern syntax. In the second stage, the named subpattern references can be used to do additional tests like the ones described above.

## Implementing clippy lints using patterns

As a "real-world" example, I re-implemented the `collapsible_if` lint using patterns. The code can be found [here](https://github.com/fkohlgrueber/rust-clippy-pattern/blob/039b07ecccaf96d6aa7504f5126720d2c9cceddd/clippy_lints/src/collapsible_if.rs#L88-L163). The pattern-based version passes all test cases that were written for `collapsible_if`.


# Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

## Overview

```
                          Pattern syntax
                                |
                                |  parsing / lowering
                                v
                           PatternTree                                
                                ^
                                |
                                |
                          IsMatch trait
                                |
                                |
             +---------------+-----------+---------+
             |               |           |         |
             v               v           v         v
        syntax::ast     rustc::hir      syn       ...
```

The pattern syntax described in the previous section is parsed / lowered into the so-called *PatternTree* data structure that represents a valid syntax tree pattern. Matching a *PatternTree* against an actual syntax tree (e.g. rust ast / hir or the syn ast, ...) is done using the *IsMatch* trait.

The *PatternTree* and the *IsMatch* trait are introduced in more detail in the following sections.

## PatternTree

The core data structure of this RFC is the **PatternTree**. 

It's a data structure similar to rust's AST / HIR, but with the following differences:

- The PatternTree doesn't contain parsing information like `Span`s
- The PatternTree can represent alternatives, sequences and optionals

The code below shows a simplified version of the current PatternTree:

> Note: The current implementation can be found [here](https://github.com/fkohlgrueber/pattern-matching/blob/dfb3bc9fbab69cec7c91e72564a63ebaa2ede638/pattern-match/src/pattern_tree.rs#L50-L96).


```
pub enum Expr {
    Lit(Alt<Lit>),
    Array(Seq<Expr>),
    Block_(Alt<BlockType>),
    If(Alt<Expr>, Alt<BlockType>, Opt<Expr>),
    IfLet(
        Alt<BlockType>,
        Opt<Expr>,
    ),
}

pub enum Lit {
    Char(Alt<char>),
    Bool(Alt<bool>),
    Int(Alt<u128>),
}

pub enum Stmt {
    Expr(Alt<Expr>),
    Semi(Alt<Expr>),
}

pub enum BlockType {
    Block(Seq<Stmt>),
}
```

The `Alt`, `Seq` and `Opt` structs look like these:

> Note: The current implementation can be found [here](https://github.com/fkohlgrueber/pattern-matching/blob/dfb3bc9fbab69cec7c91e72564a63ebaa2ede638/pattern-match/src/matchers.rs#L35-L60).

```
pub enum Alt<T> {
    Any,
    Elmt(Box<T>),
    Alt(Box<Self>, Box<Self>),
    Named(Box<Self>, ...)
}

pub enum Opt<T> {
    Any,  // anything, but not None
    Elmt(Box<T>),
    None,
    Alt(Box<Self>, Box<Self>),
    Named(Box<Self>, ...)
}

pub enum Seq<T> {
    Any,
    Empty,
    Elmt(Box<T>),
    Repeat(Box<Self>, RepeatRange),
    Seq(Box<Self>, Box<Self>),
    Alt(Box<Self>, Box<Self>),
    Named(Box<Self>, ...)
}

pub struct RepeatRange {
    pub start: usize,
    pub end: Option<usize>  // exclusive
}
```

## Parsing / Lowering

The input of a `pattern!` macro call is parsed into a `ParseTree` first and then lowered to a `PatternTree`.

Valid patterns depend on the *PatternTree* definitions. For example, the pattern `Lit(Bool(_)*)` isn't valid because the parameter type of the `Lit` variant of the `Expr` enum is `Any<Lit>` and therefore doesn't support repetition (`*`). As another example, `Array( Lit(_)* )` is a valid pattern because the parameter of `Array` is of type `Seq<Expr>` which allows sequences and repetitions.

> Note: names in the pattern syntax correspond to *PatternTree* enum **variants**. For example, the `Lit` in the pattern above refers to the `Lit` variant of the `Expr` enum (`Expr::Lit`), not the `Lit` enum.

## The IsMatch Trait

The pattern syntax and the *PatternTree* are independant of specific syntax tree implementations (rust ast / hir, syn, ...). When looking at the different pattern examples in the previous sections, it can be seen that the patterns don't contain any information specific to a certain syntax tree implementation. In contrast, clippy lints currently match against ast / hir syntax tree nodes and therefore directly depend on their implementation.

How to match the *PatternTree* against a certain syntax tree is expressed by the `IsMatch` trait (simplified implementation shown below):

```
pub trait IsMatch<O> {
    fn is_match(&self, other: &'o O) -> bool;
}
```

This trait needs to be implemented on each enum of the *PatternTree* (for the corresponding syntax tree types). For example, the `IsMatch` implementation for matching `ast::LitKind` against the *PatternTree's* `Lit` enum might look like this:

```
impl IsMatch<ast::LitKind> for Lit {
    fn is_match(&self, other: &ast::LitKind) -> bool {
        match (self, other) {
            (Lit::Char(i), ast::LitKind::Char(j)) => i.is_match(cx, j),
            (Lit::Bool(i), ast::LitKind::Bool(j)) => i.is_match(cx, j),
            (Lit::Int(i), ast::LitKind::Int(j, _)) => i.is_match(cx, j),
            _ => false,
        }
    }
}
```

All `IsMatch` implementations for matching the current *PatternTree* against `syntax::ast` can be found [here](https://github.com/fkohlgrueber/pattern-matching/blob/dfb3bc9fbab69cec7c91e72564a63ebaa2ede638/pattern-match/src/ast_match.rs).


# Drawbacks
[drawbacks]: #drawbacks

TODO: Performance
- late filtering
- currently not optimized for performance, but no conceptual limitations

- Currently, only a small part of the Rust syntax is implemented

# Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

**TODO: Advantages**

## Alternatives

### Rust-like pattern syntax

The proposed pattern syntax requires users to know the structure of the `PatternTree` (which is very similar to the AST's / HIR's structure) and also the pattern syntax. An alternative would be to introduce a pattern syntax that is similar to actual Rust syntax (probably like the `quote!` macro). For example, a pattern that matches `if` expressions that have `false` in their condition could look like this:

```
if false {
    #[*]
}
```

#### Problems

Extending Rust syntax (which is quite complex by itself) with additional syntax needed for specifying patterns (alternations, sequences, repetisions, named submatches, ...) might become difficult to read and really hard to parse properly.

For example, a pattern that matches a binary operation that has `0` on both sides might look like this:

```
0 #[*:BinOpKind] 0
```

Now consider this slightly more complex example:

```
1 + 0 #[*:BinOpKind] 0
```

The parser would need to know the precedence of `#[*:BinOpKind]` because it affects the structure of the resulting AST. `1 + 0 + 0` is parsed as `(1 + 0) + 0` while `1 + 0 * 0` is parsed as `1 + (0 * 0)`. Since the pattern could be any `BinOpKind`, the precedence cannot be known in advance.

Another example of a problem would be named submatches. Take a look at this pattern:

```
fn test() {
    1 #foo
}
```

Which node is `#foo` referring to? `int`, `ast::Lit`, `ast::Expr`, `ast::Stmt`? Naming subpatterns in a rust-like syntax is difficult because a lot of AST nodes don't have a syntactic element that can be used to put the name tag on.

In general, Rust syntax contains a lot of code structure implicitly. This structure is reconstructed during parsing (e.g. binary operations are reconstructed using operator precedence and left-to-right) and is one of the reasons why parsing is a complex task. The advantage of this approach is that writing code is simpler for users.

When writing *syntax tree patterns*, each element of the hierarchy might have alternatives, repetitions, etc.. Respecting that while still allowing human-friendly syntax that contains structure implicitly seems to be really complex, if not impossible.

Developing such a syntax would also require to maintain a custom parser that is at least as complex as the Rust parser itself. Additionally, future changes in the Rust syntax might be incompatible with such a syntax.

In summary, I think that developing such a syntax would introduce a lot of complexity to solve a relatively minor problem.

The issue of users not knowing about the *PatternTree* structure could be solved by a tool that, given a rust program, generates a pattern that matches only this program (similar to the clippy author lint).


# Prior art
[prior-art]: #prior-art

The pattern syntax is heavily inspired by regular expressions (repetitions, alternatives, sequences, ...).

From what I've seen until now, other linters also implement lints that directly work on syntax tree data structures, just like clippy does currently. I would therefore consider the pattern syntax to be *new*, but please correct me if I'm wrong.

# Unresolved questions
[unresolved-questions]: #unresolved-questions

**TODO: How to handle multiple matches?**

# Future possibilities
[future-possibilities]: #future-possibilities

**TODO: Write**

- Backreferences (`=#foo`)
- early filtering
- match descendent
- Negation operator
- Functional composition (Library of popular patterns)
