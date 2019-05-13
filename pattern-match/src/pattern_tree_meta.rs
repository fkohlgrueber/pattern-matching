use pattern_tree::gen_pattern_tree;

type STR = &'static str;

gen_pattern_tree!{
    ParseTree = Node(STR<>Ident, ParseTree*)
              | Alt(ParseTree, ParseTree)
              | Seq(ParseTree, ParseTree)
              | Repeat_(ParseTree, RepeatKind)
              | Named(ParseTree, STR<>Ident)
              | Lit(Lit)
              | Any_
              | Empty
    
    RepeatKind = Any
               | Plus
               | Optional
               | Range(u128<>LitInt, u128?<>LitInt)
               | Repeat(u128<>LitInt)

    Lit = Bool(bool<>LitBool)
        | Int(u128<>LitInt)
        | Char(char<>LitChar)
}