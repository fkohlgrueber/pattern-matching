use parse_pattern::pattern;
use lazy_static::lazy_static;

mod matchers;
mod pattern_tree;

use matchers::*;
use pattern_tree::Lit::*;
use pattern_tree::Expr::*;

pattern!(
    PAT: Alt<pattern_tree::Expr> = 
        Array( Array(()) Lit(_){1, 2} )
);

#[test]
fn test() {
    //my_print();
    println!("THIS IS THE PATTERN: {:?}", *PAT);
}

