use parse_pattern::pattern;
use lazy_static::lazy_static;

mod matchers;
mod pattern_tree;

use matchers::*;
use pattern_tree::Lit::*;
use pattern_tree::Expr::*;

pattern!(
    PAT: Opt<pattern_tree::Expr> = 
        Test(Char('a')?)
);

#[test]
fn test() {
    //my_print();
    println!("THIS IS THE PATTERN: {:?}", *PAT);
}

