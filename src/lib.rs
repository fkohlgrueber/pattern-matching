
mod parse;

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
