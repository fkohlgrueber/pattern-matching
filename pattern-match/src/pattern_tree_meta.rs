use pattern_tree::pattern_tree;

pattern_tree!{
    ParseTree = Node(String, ParseTree*)
              | Alt(ParseTree, ParseTree)
              | Seq(ParseTree, ParseTree)
              | Repeat(ParseTree, RepeatKind)
              | Named(ParseTree, String)
              | Lit(Lit)
              | Any
              | Empty
    
    RepeatKind = Any
               | Plus
               | Optional
               | Range(u128, u128?)
               | Repeat(u128)

    Lit = Bool(bool)
        | Int(u128)
        | Char(char)
}