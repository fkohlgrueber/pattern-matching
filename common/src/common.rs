use syn::{Ident, Result};
use syn::parse::{Parse, ParseStream};
use quote::{quote, ToTokens};

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum Ty {
    Alt,
    Opt,
    Seq,
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

#[test]
fn test_ord() {
    use std::cmp::{max, min};

    assert_eq!(
        max(Ty::Opt, Ty::Alt),
        Ty::Opt
    );
    assert_eq!(
        max(Ty::Opt, Ty::Seq),
        Ty::Seq
    );
    assert_eq!(
        max(Ty::Alt, Ty::Seq),
        Ty::Seq
    );
    assert_eq!(
        min(Ty::Opt, Ty::Alt),
        Ty::Alt
    );
    assert_eq!(
        min(Ty::Opt, Ty::Seq),
        Ty::Opt
    );
    assert_eq!(
        min(Ty::Alt, Ty::Seq),
        Ty::Alt
    );
}