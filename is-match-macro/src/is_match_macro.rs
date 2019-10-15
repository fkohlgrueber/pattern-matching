//! This crate contains the `derive_is_match_impl` macro that can be used to
//! implement common pattern matching code conveniently. See the 
//! `pattern-match` crate for examples.
#![recursion_limit="500"]
extern crate proc_macro;
use quote::{quote, ToTokens};
use proc_macro::TokenStream;
use syn::parse::{Parse, ParseStream};
use syn::{Result, Token};
use syn::token;
use syn::{parenthesized, braced};
use syn::punctuated::Punctuated;


struct IsMatchImpl {
    pattern_tree_node: syn::Ident,
    syntax_tree_node: syn::Path,
    match_associations_type: MatchAssociationsType,
    variants: Vec<Variant>
}

enum MatchAssociationsType {
    None, // c-style structs
    Simple, // at least one variant has parameters, but no args depend on MatchAssociations
    Some(syn::Ident) // use MatchAssociations
}

struct Variant {
    left: VariantPattern,
    right: VariantPattern
}

struct VariantPattern {
    name: syn::Ident,
    args: Vec<syn::Ident>,
    is_right: bool
}

impl Parse for VariantPattern {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse()?;
        let args = if input.peek(token::Paren) {
            let content;
            parenthesized!(content in input);
            let vals: Punctuated<syn::Ident, Token![,]>;
            vals = content.parse_terminated(syn::Ident::parse)?;
            vals.into_iter().collect()
        } else {
            vec!()
        };
        
        Ok(Self{
            name, args, is_right: false
        })
    }
}

impl Parse for Variant {
    fn parse(input: ParseStream) -> Result<Self> {
        let left = input.parse()?;
        input.parse::<Token![<]>()?;
        input.parse::<Token![>]>()?;
        let mut right: VariantPattern = input.parse()?;
        right.is_right = true;
        
        Ok(Self{
            left, right
        })
    }
}

impl Parse for IsMatchImpl {
    fn parse(input: ParseStream) -> Result<Self> {
        let pattern_tree_node = input.parse()?;
        input.parse::<Token![<]>()?;
        input.parse::<Token![>]>()?;
        let syntax_tree_node = input.parse()?;
        input.parse::<Token![=>]>()?;
        let match_associations_type = if input.peek(syn::Ident) {
            let match_associations_type = input.parse()?;
            input.parse::<Token![=>]>()?;
            MatchAssociationsType::Some(match_associations_type)
        } else if input.peek(Token![=>]) {
            input.parse::<Token![=>]>()?;
            MatchAssociationsType::Simple
        } else {
            MatchAssociationsType::None
        };
        let mut variants = vec!();
        let content;
        braced!(content in input);
        while !content.is_empty() {
            variants.push(content.parse()?);
        }
        
        Ok(Self{
            pattern_tree_node, syntax_tree_node, variants, match_associations_type
        })
    }
}

fn append_suffix(input: &syn::Ident, suffix: &str) -> syn::Ident {
    syn::Ident::new(
        &format!("{}{}", input, suffix), 
        proc_macro2::Span::call_site()
    )
}

impl ToTokens for VariantPattern {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let args = &self.args.iter().map(
            |x| append_suffix(x, if self.is_right {"_b"} else {"_a"})
        ).collect::<Vec<_>>();
        self.name.to_tokens(tokens);
        if !self.args.is_empty() {
            tokens.extend(quote!( ( #(#args),* ) ));
        }
    }
}

#[proc_macro]
pub fn derive_is_match_impl(input: TokenStream) -> TokenStream {
    
    // parse input
    let IsMatchImpl{ 
        pattern_tree_node, 
        syntax_tree_node, 
        match_associations_type,
        variants, 
    } = syn::parse_macro_input!(input as IsMatchImpl);

    // generate tokens for match arms
    
    let variant_tokens = variants.iter().map(
        |variant| {
            let Variant {left, right} = variant;
            let common_names = left.args.iter().filter(|x| right.args.contains(&x)).collect::<Vec<_>>();
        
            let rhs = if common_names.is_empty() {
                quote!( (true, cx) )
            } else if common_names.len() == 1 {
                let name_a = append_suffix(common_names[0], "_a");
                let name_b = append_suffix(common_names[0], "_b");
                quote!( 
                    #name_a .is_match(cx, #name_b)
                )
            } else {
                let is_match_tokens = common_names.iter().map(
                    |name| {
                        let name_a = append_suffix(name, "_a");
                        let name_b = append_suffix(name, "_b");
                        quote!(
                            let (r, cx) = #name_a.is_match(cx, #name_b);
                            if !r {
                                *cx = cx_orig;
                                return (false, cx);
                            }
                        )
                    }
                );
                quote!(
                    {
                        let cx_orig = cx.clone();
                        #(#is_match_tokens)*
                        (true, cx)
                    }
                )
            };

            quote!(
                (#pattern_tree_node::#left, #syntax_tree_node::#right) => #rhs,
                (#pattern_tree_node::#left, _) => (false, cx)
            )
        }).collect::<Vec<_>>();
    
    let pattern_tree_node_params = match match_associations_type {
        MatchAssociationsType::Some(t) => quote!( <'cx, 'o, Cx, #t> ),
        MatchAssociationsType::Simple => quote!( <'cx, 'o, Cx> ),
        MatchAssociationsType::None => quote!()
    };

    quote!(
        impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, #syntax_tree_node> for #pattern_tree_node #pattern_tree_node_params {
            fn is_match(&self, cx: &'cx mut Cx, other: &'o #syntax_tree_node) -> (bool, &'cx mut Cx) {
                match (self, other) {
                    
                    #(#variant_tokens),*
                }
            }
        }
    ).into()
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