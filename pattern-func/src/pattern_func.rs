#![recursion_limit="500"]
extern crate proc_macro;
use proc_macro::TokenStream;
use itertools::Itertools;
use std::str::FromStr;

pub use pattern_func_macro::pattern_func;
pub use proc_macro2;
pub use quote::quote;

use proc_macro::TokenTree;

fn is_comma(tt: &TokenTree) -> bool {
    if let TokenTree::Punct(p) = tt {
        p.as_char() == ','
    } else {
        false
    }
}

pub fn is_equals(tt: &TokenTree) -> bool {
    if let TokenTree::Punct(p) = tt {
        p.as_char() == '='
    } else {
        false
    }
}

pub fn replace(input: TokenStream, name: &str, args: &[&str], body: &str) -> TokenStream {
    let mut out = TokenStream::new();
    if input.is_empty() {
        return out;
    }

    let mut tts = input.into_iter();

    while let Some(tt) = tts.next() {
        
        out.extend(match tt {
            TokenTree::Ident(i) => {
                if i.to_string() == name {
                    //println!("found matching ident");

                    // check whether the next token is a parenthised group
                    if let Some(TokenTree::Group(g)) = tts.next() {
                        
                        // extract comma-separated arguments
                        let args_tokens = g.stream().into_iter().peekable()
                            .batching(|it| {
                                it.peek()?;  // return None if iterator is exhausted
                                let mut ts = TokenStream::new();
                                ts.extend(it.take_while(|e| !is_comma(e)));
                                Some(ts)
                            })
                            .map(|x| replace(x, name, args, body).to_string())
                            .collect::<Vec<_>>();
                        
                        if args.len() != args_tokens.len() {
                            panic!("number of args doesn't match!")
                        }

                        let mut s = body.to_string();
                        for (arg, arg_tokens) in args.iter().zip(args_tokens.iter()) {
                            s = s.replace(arg, arg_tokens)
                        }

                        TokenStream::from_str(&s).unwrap()
                    } else {
                        panic!("Expected group!")
                    }
                } else {
                    TokenStream::from(TokenTree::Ident(i))
                }
            },
            TokenTree::Group(g) => {
                TokenStream::from(TokenTree::Group(
                    proc_macro::Group::new(
                        g.delimiter(),
                        replace(g.stream(), name, args, body)
                    )
                ))
            },
            other => TokenStream::from(other)
        })

    }

    out
}

