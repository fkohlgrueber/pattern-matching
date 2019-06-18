
use pattern_tree::gen_pattern_tree;
type STR = &'static str;

gen_pattern_tree!{
    Expr = Lit(Lit)
         | If(Expr, Expr*, BlockType?)

    Lit = Char(char)
        | Bool(bool)
        | Int(u128, LitIntType)
        | Str(STR<>Symbol)

    BlockType = Block(Expr*)

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
