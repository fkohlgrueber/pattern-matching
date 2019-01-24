
extern crate proc_macro;
use proc_macro::TokenStream;

use quote::quote;

mod parse;

#[proc_macro]
pub fn pattern(item: TokenStream) -> TokenStream {
    let _input = syn::parse_macro_input!(item as parse::Expr);
    // ...

    quote!(
        static x: i32 = 0;
        /*
            fn my_print() {
                println!("test")
            }
        */
    ).into()
}

#[cfg(test)]
mod tests {

    #[test]
    fn it_works() {
        use crate::parse::Expr;

        let res = syn::parse_str::<Expr>("Foo(x, 1+2 * (z)){,2}#xy Foo() | x y");

        match res {
            Ok(i) => println!("{}", i),
            Err(e) => println!("{:?}", e),
        }
    }

    #[test]
    fn answer_test() {

    }

}
