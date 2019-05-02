
use pattern_tree::pattern_tree;

use crate::matchers::*;
use std::fmt::Debug;

pub trait MatchAssociations<'o> 
where Self: Sized + Clone {
    type Expr: 'o + Debug + Clone;
    type Lit: 'o + Debug + Clone;
    type Stmt: 'o + Debug + Clone;
    type BlockType: 'o + Debug + Clone;
    type LitIntType: 'o + Debug + Clone;
    type IntTy: 'o + Debug + Clone;
    type UintTy: 'o + Debug + Clone;
}

// Trait that has to be implemented on all types that can be used in a pattern tree
#[allow(clippy::module_name_repetitions)]
pub trait PatternTreeNode {}

impl PatternTreeNode for char {}
impl PatternTreeNode for u128 {}
impl PatternTreeNode for bool {}

impl<'cx, 'o, Cx, A> PatternTreeNode for Lit<'cx, 'o, Cx, A> where A: MatchAssociations<'o> {}
impl<'cx, 'o, Cx, A> PatternTreeNode for Expr<'cx, 'o, Cx, A> where A: MatchAssociations<'o> {}
impl<'cx, 'o, Cx, A> PatternTreeNode for Stmt<'cx, 'o, Cx, A> where A: MatchAssociations<'o> {}
impl<'cx, 'o, Cx, A> PatternTreeNode for BlockType<'cx, 'o, Cx, A> where A: MatchAssociations<'o> {}
impl<'cx, 'o, Cx, A> PatternTreeNode for LitIntType<'cx, 'o, Cx, A> where A: MatchAssociations<'o> {}
impl PatternTreeNode for IntTy {}
impl PatternTreeNode for UintTy {}


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


#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

