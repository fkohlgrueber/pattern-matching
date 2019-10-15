//! This crate provides the `pattern!()` and `meta_pattern!()` macros. These 
//! macros can be used to specify search patterns. See the macros' 
//! documentation for detailed information.
pub use pattern_macro::pattern;
pub use pattern_macro::meta_pattern;
pub use pattern_macro::pattern_mini;

pub use pattern_match;
pub use pattern_match::pattern_tree::matchers;
