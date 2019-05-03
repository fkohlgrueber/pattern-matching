#[derive(Debug)]
pub enum Expr<'cx, 'o, Cx, A>
where
    A: MatchAssociations<'o>,
{
    Lit(::pattern_tree::matchers::Alt<'cx, 'o, Lit<'cx, 'o, Cx, A>, Cx, A::Lit>),
    Array(::pattern_tree::matchers::Seq<'cx, 'o, Expr<'cx, 'o, Cx, A>, Cx, A::Expr>),
    Block_(::pattern_tree::matchers::Alt<'cx, 'o, BlockType<'cx, 'o, Cx, A>, Cx, A::BlockType>),
    If(
        ::pattern_tree::matchers::Alt<'cx, 'o, Expr<'cx, 'o, Cx, A>, Cx, A::Expr>,
        ::pattern_tree::matchers::Alt<'cx, 'o, BlockType<'cx, 'o, Cx, A>, Cx, A::BlockType>,
        ::pattern_tree::matchers::Opt<'cx, 'o, Expr<'cx, 'o, Cx, A>, Cx, A::Expr>,
    ),
    IfLet(
        ::pattern_tree::matchers::Alt<'cx, 'o, BlockType<'cx, 'o, Cx, A>, Cx, A::BlockType>,
        ::pattern_tree::matchers::Opt<'cx, 'o, Expr<'cx, 'o, Cx, A>, Cx, A::Expr>,
    ),
}
impl<'cx, 'o, Cx, A> ::pattern_tree::PatternTreeNode for Expr<'cx, 'o, Cx, A> where
    A: MatchAssociations<'o>
{
}

#[derive(Debug)]
pub enum Lit<'cx, 'o, Cx, A>
where
    A: MatchAssociations<'o>,
{
    Char(::pattern_tree::matchers::Alt<'cx, 'o, char, Cx, char>),
    Bool(::pattern_tree::matchers::Alt<'cx, 'o, bool, Cx, bool>),
    Int(
        ::pattern_tree::matchers::Alt<'cx, 'o, u128, Cx, u128>,
        ::pattern_tree::matchers::Alt<'cx, 'o, LitIntType<'cx, 'o, Cx, A>, Cx, A::LitIntType>,
    ),
    Str(::pattern_tree::matchers::Alt<'cx, 'o, STR, Cx, A::Symbol>),
}
impl<'cx, 'o, Cx, A> ::pattern_tree::PatternTreeNode for Lit<'cx, 'o, Cx, A> where
    A: MatchAssociations<'o>
{
}

#[derive(Debug)]
pub enum BlockType<'cx, 'o, Cx, A>
where
    A: MatchAssociations<'o>,
{
    Block(::pattern_tree::matchers::Seq<'cx, 'o, Stmt<'cx, 'o, Cx, A>, Cx, A::Stmt>),
}
impl<'cx, 'o, Cx, A> ::pattern_tree::PatternTreeNode for BlockType<'cx, 'o, Cx, A> where
    A: MatchAssociations<'o>
{
}

#[derive(Debug)]
pub enum Stmt<'cx, 'o, Cx, A>
where
    A: MatchAssociations<'o>,
{
    Expr(::pattern_tree::matchers::Alt<'cx, 'o, Expr<'cx, 'o, Cx, A>, Cx, A::Expr>),
    Semi(::pattern_tree::matchers::Alt<'cx, 'o, Expr<'cx, 'o, Cx, A>, Cx, A::Expr>),
}
impl<'cx, 'o, Cx, A> ::pattern_tree::PatternTreeNode for Stmt<'cx, 'o, Cx, A> where
    A: MatchAssociations<'o>
{
}

#[derive(Debug)]
pub enum LitIntType<'cx, 'o, Cx, A>
where
    A: MatchAssociations<'o>,
{
    Signed(::pattern_tree::matchers::Alt<'cx, 'o, IntTy, Cx, A::IntTy>),
    Unsigned(::pattern_tree::matchers::Alt<'cx, 'o, UintTy, Cx, A::UintTy>),
    Unsuffixed,
}
impl<'cx, 'o, Cx, A> ::pattern_tree::PatternTreeNode for LitIntType<'cx, 'o, Cx, A> where
    A: MatchAssociations<'o>
{
}

#[derive(Debug)]
pub enum IntTy {
    Isize,
    I8,
    I16,
    I32,
    I64,
    I128,
}
impl ::pattern_tree::PatternTreeNode for IntTy {}

#[derive(Debug)]
pub enum UintTy {
    Usize,
    U8,
    U16,
    U32,
    U64,
    U128,
}
impl ::pattern_tree::PatternTreeNode for UintTy {}

pattern_tree::lazy_static! {
    pub static ref TYPES: rustc_data_structures::fx::FxHashMap<&'static str, Vec<(&'static str, pattern_tree::Ty)>> = {
        let mut p = rustc_data_structures::fx::FxHashMap::default();
        p.insert(
            "Lit",
            vec![("Lit", pattern_tree::Ty::Alt)],
        );
        p.insert(
            "Array",
            vec![("Expr", pattern_tree::Ty::Seq)],
        );
        p.insert(
            "Block_",
            vec![("BlockType", pattern_tree::Ty::Alt)],
        );
        p.insert(
            "If",
            vec![
                ("Expr", pattern_tree::Ty::Alt),
                ("BlockType", pattern_tree::Ty::Alt),
                ("Expr", pattern_tree::Ty::Opt),
            ],
        );
        p.insert(
            "IfLet",
            vec![
                ("BlockType", pattern_tree::Ty::Alt),
                ("Expr", pattern_tree::Ty::Opt),
            ],
        );
        p.insert(
            "Char",
            vec![("char", pattern_tree::Ty::Alt)],
        );
        p.insert(
            "Bool",
            vec![("bool", pattern_tree::Ty::Alt)],
        );
        p.insert(
            "Int",
            vec![
                ("u128", pattern_tree::Ty::Alt),
                ("LitIntType", pattern_tree::Ty::Alt),
            ],
        );
        p.insert(
            "Str",
            vec![("Symbol", pattern_tree::Ty::Alt)],
        );
        p.insert(
            "Block",
            vec![("Stmt", pattern_tree::Ty::Seq)],
        );
        p.insert(
            "Expr",
            vec![("Expr", pattern_tree::Ty::Alt)],
        );
        p.insert(
            "Semi",
            vec![("Expr", pattern_tree::Ty::Alt)],
        );
        p.insert(
            "Signed",
            vec![("IntTy", pattern_tree::Ty::Alt)],
        );
        p.insert(
            "Unsigned",
            vec![("UintTy", pattern_tree::Ty::Alt)],
        );
        p.insert("Unsuffixed", vec![]);
        p.insert("Isize", vec![]);
        p.insert("I8", vec![]);
        p.insert("I16", vec![]);
        p.insert("I32", vec![]);
        p.insert("I64", vec![]);
        p.insert("I128", vec![]);
        p.insert("Usize", vec![]);
        p.insert("U8", vec![]);
        p.insert("U16", vec![]);
        p.insert("U32", vec![]);
        p.insert("U64", vec![]);
        p.insert("U128", vec![]); 
    }; 
}

pub mod variants {
    pub use super::BlockType::*;
    pub use super::Expr::*;
    pub use super::IntTy::*;
    pub use super::Lit::*;
    pub use super::LitIntType::*;
    pub use super::Stmt::*;
    pub use super::UintTy::*;
}

pub trait MatchAssociations<'o>
where
    Self: Sized + Clone,
{
    type Expr: 'o + std::fmt::Debug + Clone;
    type Lit: 'o + std::fmt::Debug + Clone;
    type BlockType: 'o + std::fmt::Debug + Clone;
    type Stmt: 'o + std::fmt::Debug + Clone;
    type LitIntType: 'o + std::fmt::Debug + Clone;
    type IntTy: 'o + std::fmt::Debug + Clone;
    type UintTy: 'o + std::fmt::Debug + Clone;
    type Symbol: 'o + std::fmt::Debug + Clone;
}
