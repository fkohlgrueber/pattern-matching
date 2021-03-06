
use pattern_tree::gen_pattern_tree;

gen_pattern_tree!{
    Expr = Lit(Lit)
         | Array(Expr*)
         | If(Expr, BlockType, Expr?)
         | Block_(BlockType)

    BlockType = Block(Stmt*)

    Lit = Char(char)
        | Bool(bool)

    Stmt = Expr(Expr)
         | Semi(Expr)
}