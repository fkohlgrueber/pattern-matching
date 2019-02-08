
use gen_pattern_tree::gen_pattern_tree;

use lazy_static::lazy_static;

pub mod matchers;

use matchers::*;

gen_pattern_tree!();




#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
