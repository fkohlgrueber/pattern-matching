use parse_pattern::pattern;

pattern!(
    Foo(x, 1+2 * (z)){,2}#xy Foo() | x y
);


#[test]
fn test() {
    //my_print();

    println!("{}", x);
}

