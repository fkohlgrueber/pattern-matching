
use pattern::pattern;

use lazy_static::lazy_static;

use pattern_tree::matchers::*;
use pattern_tree::Expr::*;
use pattern_tree::Lit::*;

pattern!(
    PAT: pattern_tree::Expr = 
        Array( Array(()) Lit(Bool(_)){1, 2} )
);


#[test]
fn test() {
    println!("THIS IS THE PATTERN: {:?}", *PAT);
}

