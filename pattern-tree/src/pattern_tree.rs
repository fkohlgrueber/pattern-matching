
pub use pattern_tree_macro::gen_pattern_tree;
pub use lazy_static::lazy_static;
pub use common::Ty;

pub mod matchers;

// Trait that has to be implemented on all types that can be used in a pattern tree
pub trait PatternTreeNode {}

impl PatternTreeNode for char {}
impl PatternTreeNode for u128 {}
impl PatternTreeNode for bool {}
impl PatternTreeNode for &'static str {}