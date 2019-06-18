
//
// Patter Tree implementations
//


pub enum Expr<'cx, 'o, Cx, A>
where A: MatchAssociations<'o> {
    Lit(Alt<'cx, 'o, Lit<'cx, 'o, Cx, A>, Cx, A::Lit>),
    Array(Seq<'cx, 'o, Expr<'cx, 'o, Cx, A>, Cx, A::Expr>),
    Block_(Alt<'cx, 'o, BlockType<'cx, 'o, Cx, A>, Cx, A::BlockType>),
    If(Alt<'cx, 'o, Expr<'cx, 'o, Cx, A>, Cx, A::Expr>,
       Alt<'cx, 'o, BlockType<'cx, 'o, Cx, A>, Cx, A::BlockType>,
       Opt<'cx, 'o, Expr<'cx, 'o, Cx, A>, Cx, A::Expr>),
    IfLet(Alt<'cx, 'o, BlockType<'cx, 'o, Cx, A>, Cx, A::BlockType>,
          Opt<'cx, 'o, Expr<'cx, 'o, Cx, A>, Cx, A::Expr>)
}

pub enum Lit<'cx, 'o, Cx, A>
where A: MatchAssociations<'o> {
    Char(Alt<'cx, 'o, char, Cx, char>),
    Bool(Alt<'cx, 'o, bool, Cx, bool>),
    Int(Alt<'cx, 'o, u128, Cx, u128>,
        Alt<'cx, 'o, LitIntType<'cx, 'o, Cx, A>, Cx, A::LitIntType>),
    Str(Alt<'cx, 'o, STR, Cx, A::Symbol>)
}

pub enum BlockType<'cx, 'o, Cx, A>
where A: MatchAssociations<'o> {
    Block(Seq<'cx, 'o, Stmt<'cx, 'o, Cx, A>, Cx, A::Stmt>)
}

pub enum Stmt<'cx, 'o, Cx, A>
where A: MatchAssociations<'o> {
    Expr(Alt<'cx, 'o, Expr<'cx, 'o, Cx, A>, Cx, A::Expr>),
    Semi(Alt<'cx, 'o, Expr<'cx, 'o, Cx, A>, Cx, A::Expr>)
}

pub enum LitIntType<'cx, 'o, Cx, A>
where A: MatchAssociations<'o> {
    Signed(Alt<'cx, 'o, IntTy, Cx, A::IntTy>),
    Unsigned(Alt<'cx, 'o, UintTy, Cx, A::UintTy>),
    Unsuffixed
}

pub enum IntTy {
    Isize, I8, I16, I32, I64, I128
}

pub enum UintTy {
    Usize, U8, U16, U32, U64, U128
}

//
// PatternTreeNode trait implementations
//

impl<'cx, 'o, Cx, A> PatternTreeNode for Expr<'cx, 'o, Cx, A> where A: MatchAssociations<'o> {}
impl<'cx, 'o, Cx, A> PatternTreeNode for Lit<'cx, 'o, Cx, A> where A: MatchAssociations<'o> {}
impl<'cx, 'o, Cx, A> PatternTreeNode for BlockType<'cx, 'o, Cx, A> where A: MatchAssociations<'o> {}
impl<'cx, 'o, Cx, A> PatternTreeNode for Stmt<'cx, 'o, Cx, A> where A: MatchAssociations<'o> {}
impl<'cx, 'o, Cx, A> PatternTreeNode for LitIntType<'cx, 'o, Cx, A> where A: MatchAssociations<'o> {}
impl PatternTreeNode for IntTy {}
impl PatternTreeNode for UintTy {}


//
// Type information for each variant
//

pattern_tree::lazy_static! {
    pub static ref TYPES: rustc_data_structures::fx::FxHashMap<&'static str, Vec<(&'static str, Ty)>> = {
        let mut p = rustc_data_structures::fx::FxHashMap::default();
        p.insert("Lit", vec![("Lit", Ty::Alt)]);
        p.insert("Array", vec![("Expr", Ty::Seq)]);
        p.insert("Block_", vec![("BlockType", Ty::Alt)]);
        p.insert("If", vec![("Expr", Ty::Alt),
                            ("BlockType", Ty::Alt),
                            ("Expr", Ty::Opt)]);
        p.insert("IfLet", vec![("BlockType", Ty::Alt),
                               ("Expr", Ty::Opt)]);
        p.insert("Char", vec![("char", Ty::Alt)]);
        p.insert("Bool", vec![("bool", Ty::Alt)]);
        p.insert("Int", vec![("u128", Ty::Alt),
                             ("LitIntType", Ty::Alt)]);
        p.insert("Str", vec![("Symbol", Ty::Alt)]);
        p.insert("Block", vec![("Stmt", Ty::Seq)]);
        p.insert("Expr", vec![("Expr", Ty::Alt)]);
        p.insert("Semi", vec![("Expr", Ty::Alt)]);
        p.insert("Signed", vec![("IntTy", Ty::Alt)]);
        p.insert("Unsigned", vec![("UintTy", Ty::Alt)]);
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

//
// import helpers
//

pub mod variants {
    pub use super::BlockType::*;
    pub use super::Expr::*;
    pub use super::IntTy::*;
    pub use super::Lit::*;
    pub use super::LitIntType::*;
    pub use super::Stmt::*;
    pub use super::UintTy::*;
}

//
// MatchAssociations trait
//

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
