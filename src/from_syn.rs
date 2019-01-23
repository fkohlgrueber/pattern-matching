use syn::Ident;

pub enum BinOp {
    Seq,
    Alt
}

pub enum Expr {
    Binary(Box<Expr>, BinOp, Box<Expr>),
    Ident(Ident)
}

impl Parse for Expr {
    fn parse(input: ParseStream) -> Result<Self> {
        ambiguous_expr(input, AllowStruct(true))
    }
}