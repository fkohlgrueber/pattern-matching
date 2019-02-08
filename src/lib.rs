
use pattern::pattern;

use lazy_static::lazy_static;

pattern!(
    PAT: pattern_tree::Expr = 
        Array(
            Array(_) | Test(_)#test | Lit(Bool(_) | Int(_) | Char(_)) |
            Array( Array(()) Lit(Bool(_)){1, 2} Test(_?))
        )
);


#[test]
fn test() {
    println!("THIS IS THE PATTERN: {:?}", *PAT);
}

