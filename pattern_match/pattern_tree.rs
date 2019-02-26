#![recursion_limit="256"]

use gen_pattern_tree::gen_pattern_tree;

use lazy_static::lazy_static;

use crate::matchers::*;

use std::fmt::Debug;

pub trait MatchAssociations {
    type Expr: Debug;
    type Lit: Debug;
    type Bool: Debug;
    type Char: Debug;
    type Int: Debug;
    type Stmt: Debug;
    type BlockType: Debug;
}


gen_pattern_tree!();




#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
