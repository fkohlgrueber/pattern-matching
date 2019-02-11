
use pattern::pattern;

use lazy_static::lazy_static;

pattern!(
    PAT: pattern_tree::Expr = 
        Array(
            Array(_) | Lit(Bool(_) | Int(_) | Char(_)) |
            Array( Array(()) Lit(Bool(_)){1, 2} )
        )
);


#[test]
fn test() {
    println!("THIS IS THE PATTERN: {:?}", *PAT);
}

