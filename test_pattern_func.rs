#[proc_macro]
pub fn expr_or_semi(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let name = "expr_or_semi";
    let args = ["$expr"];
    let body = "(Expr ( $expr ) | Semi ( $expr ))";
    let mut tokens_iter = input.into_iter(); 
    let pattern_name = proc_macro2::TokenStream::from(proc_macro::TokenStream::from(tokens_iter.next().unwrap())); 
    let pre = {
        let mut p = proc_macro::TokenStream::new();
        p.extend(tokens_iter.by_ref().take_while(
            |x| !pattern_func::is_equals(x))
        ); 
        proc_macro2::TokenStream::from(p)
    }; 
    let pattern = {
        let mut p = proc_macro::TokenStream::new();
        p.extend(tokens_iter); 
        p 
    }; 
    let out = proc_macro2::TokenStream::from(
        pattern_func::replace(pattern, name, &args, body)
    ); 
    quote!( #pattern_name !{ #pre = #out } ).into()
}

