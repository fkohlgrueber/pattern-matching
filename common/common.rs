use syn::{Ident, Result};
use syn::parse::{Parse, ParseStream};
use quote::{quote, ToTokens};

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Alt,
    Seq,
    Opt
}

impl ToTokens for Ty {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        tokens.extend(
            match self {
                Ty::Alt => quote!(Alt),
                Ty::Seq => quote!(Seq),
                Ty::Opt => quote!(Opt)
            }
        )
    }
}

impl Parse for Ty {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse::<Ident>()?;
        if name == "Alt" {
            Ok(Ty::Alt)
        } else if name == "Seq" {
            Ok(Ty::Seq)
        } else if name == "Opt" {
            Ok(Ty::Opt)
        } else {
            Err(input.error("expected Alt, Seq or Opt"))
        }
    }
}