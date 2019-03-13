
use pattern_tree::pattern_tree;

use crate::matchers::*;
use std::fmt::Debug;

pub trait MatchAssociations<'o> 
where Self: Sized + Clone {
    type Expr: 'o + Debug + Clone;
    type Lit: 'o + Debug + Clone;
    type Bool: 'o + Debug + Clone;
    type Char: 'o + Debug + Clone;
    type Int: 'o + Debug + Clone;
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


/*
Expr = Lit(Lit)
     | Array(Expr*)
     | Block_(BlockType)
     | If(Expr, BlockType, Expr?)
     | IfLet(BlockType, Expr?)

Lit = Char(char)
    | Bool(bool)
    | Int(u128)

BlockType = Block(Stmt*)

Stmt = Expr(Expr)
     | Semi(Expr)
*/


pattern_tree!{
    #[derive(Debug)]
    pub enum Expr<'cx, 'o, Cx, A>
    where
        A: MatchAssociations<'o>,
    {
        Lit(Alt<'cx, 'o, Lit<'cx, 'o, Cx, A>, Cx, A::Lit>),
        Array(Seq<'cx, 'o, Expr<'cx, 'o, Cx, A>, Cx, A::Expr>),
        Block_(Alt<'cx, 'o, BlockType<'cx, 'o, Cx, A>, Cx, A::BlockType>),
        If(
            Alt<'cx, 'o, Expr<'cx, 'o, Cx, A>, Cx, A::Expr>,
            Alt<'cx, 'o, BlockType<'cx, 'o, Cx, A>, Cx, A::BlockType>,
            Opt<'cx, 'o, Expr<'cx, 'o, Cx, A>, Cx, A::Expr>,
        ),
        IfLet(
            Alt<'cx, 'o, BlockType<'cx, 'o, Cx, A>, Cx, A::BlockType>,
            Opt<'cx, 'o, Expr<'cx, 'o, Cx, A>, Cx, A::Expr>,
        ),
    }

    #[derive(Debug)]
    pub enum Lit<'cx, 'o, Cx, A>
    where
        A: MatchAssociations<'o>,
    {
        Char(Alt<'cx, 'o, char, Cx, A::Char>),
        Bool(Alt<'cx, 'o, bool, Cx, A::Bool>),
        Int(
            Alt<'cx, 'o, u128, Cx, A::Int>, 
            Alt<'cx, 'o, LitIntType<'cx, 'o, Cx, A>, Cx, A::LitIntType>
        ),
    }

    #[derive(Debug)]
    pub enum Stmt<'cx, 'o, Cx, A>
    where
        A: MatchAssociations<'o>,
    {
        Expr(Alt<'cx, 'o, Expr<'cx, 'o, Cx, A>, Cx, A::Expr>),
        Semi(Alt<'cx, 'o, Expr<'cx, 'o, Cx, A>, Cx, A::Expr>),
    }

    #[derive(Debug)]
    pub enum BlockType<'cx, 'o, Cx, A>
    where
        A: MatchAssociations<'o>,
    {
        Block(Seq<'cx, 'o, Stmt<'cx, 'o, Cx, A>, Cx, A::Stmt>),
    }

    #[derive(Debug)]
    pub enum LitIntType<'cx, 'o, Cx, A>
    where
        A: MatchAssociations<'o>,
    {
        Signed(Alt<'cx, 'o, IntTy, Cx, A::IntTy>),
        Unsigned(Alt<'cx, 'o, UintTy, Cx, A::UintTy>),
        Unsuffixed,
    }

    #[derive(Debug)]
    pub enum IntTy
    {
        Isize,
        I8,
        I16,
        I32,
        I64,
        I128,
    }

    #[derive(Debug)]
    pub enum UintTy {
        Usize,
        U8,
        U16,
        U32,
        U64,
        U128,
    }
}




#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
