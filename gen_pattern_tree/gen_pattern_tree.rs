extern crate proc_macro;
use quote::{quote, ToTokens};
use proc_macro::TokenStream;

enum Ty {
    Alt,
    Seq,
    Opt
}

struct PatternTreeNode {
    name: syn::Ident,
    variants: Vec<PatternTreeVariant>
}

struct PatternTreeVariant {
    name: syn::Ident,
    args: Vec<PatternTreeArg>
}

struct PatternTreeArg {
    ty: Ty,
    inner_ty: syn::Ident,
    inner_ty_primitive: bool,
    assoc_ty: syn::Ident
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

impl ToTokens for PatternTreeArg {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let ty = &self.ty;
        let inner_ty = &self.inner_ty;
        let inner_ty = if self.inner_ty_primitive {
            quote!(#inner_ty)
        } else {
            quote!(#inner_ty<'cx, 'o, Cx, A>)
        };
        let assoc_ty = &self.assoc_ty;
        tokens.extend(quote!(#ty < 'cx, 'o, #inner_ty, Cx, A::#assoc_ty >))
    }
}

impl ToTokens for PatternTreeVariant {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let name = &self.name;
        let args = &self.args;
        tokens.extend(quote!(#name (#(#args),* )))
    }
}

impl ToTokens for PatternTreeNode {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let name = &self.name;
        let variants = &self.variants;
        tokens.extend(quote!(
            #[derive(Debug)]
            pub enum #name<'cx, 'o, Cx, A>
            where A: MatchAssociations<'o> {
                #(#variants),*
            }
        ))
    }
}


struct Enums(Vec<syn::ItemEnum>);

use syn::parse::{Parse, ParseStream};
use syn::{Result};
impl Parse for Enums {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut res = vec!();
        while !input.is_empty() {
            res.push(input.parse()?);
        }
        Ok(
            Enums(res)
        )
    }
}

impl ToTokens for Enums {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        for e in &self.0 {
            e.to_tokens(tokens)
        }
    }
}


#[proc_macro]
pub fn gen_pattern_tree(input: TokenStream) -> TokenStream {
    
    // parse input
    let enums = syn::parse_macro_input!(input as Enums);

    // extract relevant information about the enums' variants, their arguments and types
    // TODO: improve error handling!
    let mut pattern_tree_def = vec!();
    for e in &enums.0 {
        let mut variants = vec!();
        for v in &e.variants {
            let mut params = vec!();
            if let syn::Fields::Unnamed(f) = &v.fields {
                for field in &f.unnamed {
                    if let syn::Type::Path(p) = &field.ty {
                        assert!(p.path.segments.len() == 1);
                        let path_segment = &p.path.segments[0];
                        let repeat_ty = path_segment.ident.to_string();
                        let mut type_idents = vec!();
                        if let syn::PathArguments::AngleBracketed(abga) = &path_segment.arguments {
                            for generic_argument in &abga.args {
                                if let syn::GenericArgument::Type(t) = generic_argument {
                                    if let syn::Type::Path(p) = &t {
                                        for path_segment in &p.path.segments {
                                            if path_segment.ident != "A" && path_segment.ident != "Cx" {
                                                type_idents.push(path_segment.ident.clone())
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        assert!(type_idents.len() == 2);
                        params.push(PatternTreeArg { 
                            ty: match repeat_ty.as_str() {
                                "Alt" => Ty::Alt,
                                "Seq" => Ty::Seq,
                                "Opt" => Ty::Opt,
                                _ => panic!("wrong type!")
                            },
                            inner_ty_primitive: type_idents[0].to_string() != type_idents[1].to_string(),
                            inner_ty: type_idents[0].clone(),
                            assoc_ty: type_idents[1].clone(),
                        });
                    }
                }
            }
            variants.push(
                PatternTreeVariant {
                    name: v.ident.clone(), 
                    args: params
                }
            );
        }
        pattern_tree_def.push(
            PatternTreeNode {
                name: e.ident.clone(),
                variants
            }
        )
    }
    
    // generate type information struct
    let x = pattern_tree_def.iter().flat_map(|x| &x.variants).map(
        |x| {
            let name = &x.name;
            let args = x.args.iter().map(|x| &x.ty).collect::<Vec<_>>();
            let args_id = x.args.iter().map(|x| &x.assoc_ty).collect::<Vec<_>>();
            quote!( p.insert(stringify!(#name), vec!(#( (stringify!(#args_id), Ty::#args) ),*)); )
        }
    ).collect::<Vec<_>>();
    let types = quote!(
        lazy_static!{
            pub static ref TYPES: std::collections::HashMap<&'static str, Vec<(&'static str, Ty)>> = {
                let mut p = std::collections::HashMap::new();
                #(#x)*
                p
            };
        }
    );

    // generate mod that exports all enum variants
    let enum_names = pattern_tree_def.iter().map(|x| &x.name).collect::<Vec<_>>();
    let exports = quote!(
        pub mod variants {
            #(
                pub use super:: #enum_names ::*;
            )*
        }
    );
    
    quote!(
        pub enum Ty {
            Alt,
            Seq,
            Opt
        }
        #enums 
        #types
        #exports
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