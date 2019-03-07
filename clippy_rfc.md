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

# Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

## PatternTree

The core data structure of this RFC is the **PatternTree**. 

It's a data structure similar to rust's AST / HIR, but with the following differences:

- The PatternTree doesn't contain parsing information like `Span`s
- The PatternTree can represent alternatives, sequences and optionals

The code below shows a simplified version of the current PatternTree:

*Note: The current implementation can be found [here](https://github.com/fkohlgrueber/pattern-matching/blob/dfb3bc9fbab69cec7c91e72564a63ebaa2ede638/pattern-match/src/pattern_tree.rs#L50-L96).*


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



This is the technical portion of the RFC. Explain the design in sufficient detail that:

- Its interaction with other features is clear.
- It is reasonably clear how the feature would be implemented.
- Corner cases are dissected by example.

The section should return to the examples given in the previous section, and explain more fully how the detailed proposal makes those examples work.

# Drawbacks
[drawbacks]: #drawbacks

Why should we *not* do this?

# Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

- Why is this design the best in the space of possible designs?
- What other designs have been considered and what is the rationale for not choosing them?
- What is the impact of not doing this?

# Prior art
[prior-art]: #prior-art

Discuss prior art, both the good and the bad, in relation to this proposal.
A few examples of what this can include are:

- For language, library, cargo, tools, and compiler proposals: Does this feature exist in other programming languages and what experience have their community had?
- For community proposals: Is this done by some other community and what were their experiences with it?
- For other teams: What lessons can we learn from what other communities have done here?
- Papers: Are there any published papers or great posts that discuss this? If you have some relevant papers to refer to, this can serve as a more detailed theoretical background.

This section is intended to encourage you as an author to think about the lessons from other languages, provide readers of your RFC with a fuller picture.
If there is no prior art, that is fine - your ideas are interesting to us whether they are brand new or if it is an adaptation from other languages.

Note that while precedent set by other languages is some motivation, it does not on its own motivate an RFC.
Please also take into consideration that rust sometimes intentionally diverges from common language features.

# Unresolved questions
[unresolved-questions]: #unresolved-questions

- What parts of the design do you expect to resolve through the RFC process before this gets merged?
- What parts of the design do you expect to resolve through the implementation of this feature before stabilization?
- What related issues do you consider out of scope for this RFC that could be addressed in the future independently of the solution that comes out of this RFC?

# Future possibilities
[future-possibilities]: #future-possibilities

Think about what the natural extension and evolution of your proposal would
be and how it would affect the language and project as a whole in a holistic
way. Try to use this section as a tool to more fully consider all possible
interactions with the project and language in your proposal.
Also consider how the this all fits into the roadmap for the project
and of the relevant sub-team.

This is also a good place to "dump ideas", if they are out of scope for the
RFC you are writing but otherwise related.

If you have tried and cannot think of any future possibilities,
you may simply state that you cannot think of anything.

Note that having something written down in the future-possibilities section
is not a reason to accept the current or a future RFC; such notes should be
in the section on motivation or rationale in this or subsequent RFCs.
The section merely provides additional information.

