
pub use pattern_tree_macro::pattern_tree;
pub use lazy_static::lazy_static;
pub use common::Ty;


// Trait that has to be implemented on all types that can be used in a pattern tree
pub trait PatternTreeNode {}

impl PatternTreeNode for char {}
impl PatternTreeNode for u128 {}
impl PatternTreeNode for bool {}