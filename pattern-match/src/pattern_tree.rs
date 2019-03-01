
use pattern_tree::pattern_tree;

use crate::matchers::*;
use std::fmt::Debug;

pub trait MatchAssociations<'o> 
where Self: Sized {
    type Expr: 'o + Debug;
    type Lit: 'o + Debug;
    type Bool: 'o + Debug;
    type Char: 'o + Debug;
    type Int: 'o + Debug;
    type Stmt: 'o + Debug;
    type BlockType: 'o + Debug;
}

// Trait that has to be implemented on all types that can be used in a pattern tree
pub trait PatternTreeNode {}

impl PatternTreeNode for char {}
impl PatternTreeNode for u128 {}
impl PatternTreeNode for bool {}

impl<'cx, 'o, Cx, A> PatternTreeNode for Lit<'cx, 'o, Cx, A> where A: MatchAssociations<'o> {}
impl<'cx, 'o, Cx, A> PatternTreeNode for Expr<'cx, 'o, Cx, A> where A: MatchAssociations<'o> {}
impl<'cx, 'o, Cx, A> PatternTreeNode for Stmt<'cx, 'o, Cx, A> where A: MatchAssociations<'o> {}
impl<'cx, 'o, Cx, A> PatternTreeNode for BlockType<'cx, 'o, Cx, A> where A: MatchAssociations<'o> {}


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
        Int(Alt<'cx, 'o, u128, Cx, A::Int>),
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
}




#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
