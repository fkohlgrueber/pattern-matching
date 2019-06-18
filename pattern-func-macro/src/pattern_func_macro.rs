#![recursion_limit="500"]
extern crate proc_macro;
use quote::quote;
use proc_macro::TokenStream;
use syn::parse::{Parse, ParseStream};
use syn::{Result, Token};
use syn::parenthesized;
use syn::punctuated::Punctuated;


#[derive(Debug)]
struct FuncDef {
    name: syn::Ident,
    args: Vec<syn::Ident>,
    body: proc_macro2::TokenStream
}

struct Param(syn::Ident);

impl Parse for Param {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<syn::token::Dollar>()?;
        Ok(Param(input.parse()?))
    }
}

impl Parse for FuncDef {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![fn]>()?;
        let name = input.parse()?;
        
        let content;
        parenthesized!(content in input);
        let vals: Punctuated<Param, Token![,]>;
        vals = content.parse_terminated(Param::parse)?;
        let args = vals.into_iter().map(|x| x.0).collect();

        let body = parse_body(input)?;
                
        Ok(Self{
            name, args, body
        })
    }
}

fn parse_body(input: ParseStream) -> Result<proc_macro2::TokenStream> {
    input.step(|cursor| {
        if let Some((proc_macro2::TokenTree::Group(g), rest)) = cursor.token_tree() {
            Ok((g.stream().clone(), rest))
        } else {
            Err(cursor.error("expected body"))
        }
    })
}

#[proc_macro]
pub fn pattern_func(input: TokenStream) -> TokenStream {
    
    // parse input
    let func_def = syn::parse_macro_input!(input as FuncDef);
    
    let name = func_def.name;
    let name_str = name.to_string();
    let args = func_def.args.iter().map(|x| {
        let mut s = "$".to_string(); 
        s.push_str(x.to_string().as_str()); 
        s
    }).collect::<Vec<_>>();
    let body = format!("({})", func_def.body).replace("$ ", "$");

    let hash = proc_macro2::TokenTree::Punct(proc_macro2::Punct::new('#', proc_macro2::Spacing::Alone));

    let tokens = quote!(
        #[proc_macro]
        pub fn #name(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
            
            let name = #name_str;
            let args = [#(#args),*];
            let body = #body;
            
            let mut tokens_iter = input.into_iter();
            let pattern_name = pattern_func::proc_macro2::TokenStream::from(proc_macro::TokenStream::from(tokens_iter.next().unwrap()));

            let pre = {
                let mut p = proc_macro::TokenStream::new();
                p.extend(tokens_iter.by_ref().take_while(|x| !pattern_func::is_equals(x)));
                pattern_func::proc_macro2::TokenStream::from(p)
            };
            let pattern = {
                let mut p = proc_macro::TokenStream::new();
                p.extend(tokens_iter);
                p
            };

            let out = pattern_func::proc_macro2::TokenStream::from(pattern_func::replace(pattern, name, &args, body));

            let tokens = pattern_func::quote!(
                #hash pattern_name !{
                    #hash pre = 
                        #hash out
                }
            );
            println!("Expanded to: \n\n {}\n\n\n", tokens.to_string());
            tokens.into()
        }
        
    );
    //println!("Pattern_func: \n\n {}\n\n\n", tokens.clone().to_string());
    tokens.into()
}

/*
#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
*/