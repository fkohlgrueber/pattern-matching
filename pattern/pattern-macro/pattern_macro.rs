//! This crate defines several pattern procedural macro. This crate is
//! not supposed to be used directly. use the `pattern` crate instead.
#![recursion_limit="512"]

extern crate proc_macro;
use proc_macro::TokenStream;

use pattern_macro_gen::gen_pattern_macro;
use pattern_match;

gen_pattern_macro!{
    pattern => pattern_match::pattern_tree_rust
}

gen_pattern_macro!{
    meta_pattern => pattern_match::pattern_tree_meta
}
