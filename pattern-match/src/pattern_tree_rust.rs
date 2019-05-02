
use pattern_tree::pattern_tree;

pattern_tree!{
    Expr = Lit(Lit)
        | Array(Expr*)
        | Block_(BlockType)
        | If(Expr, BlockType, Expr?)
        | IfLet(BlockType, Expr?)

    Lit = Char(char)
        | Bool(bool)
        | Int(u128, LitIntType)

    BlockType = Block(Stmt*)

    Stmt = Expr(Expr)
        | Semi(Expr)

    LitIntType = Signed(IntTy)
            | Unsigned(UintTy)
            | Unsuffixed

    IntTy = Isize
        | I8
        | I16
        | I32
        | I64
        | I128

    UintTy = Usize
        | U8
        | U16
        | U32
        | U64
        | U128
}
