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
    inner_ty: syn::Ident
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
        tokens.extend(quote!(#ty < #inner_ty >))
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
            pub enum #name {
                #(#variants),*
            }
        ))
    }
}

fn ident(input: &'static str) -> syn::Ident {
    syn::Ident::new(input, proc_macro2::Span::call_site())
}


#[proc_macro]
pub fn gen_pattern_tree(_item: TokenStream) -> TokenStream {
    let pattern_tree_def = vec!(
        PatternTreeNode {
            name: ident("Expr"),
            variants: vec!(
                PatternTreeVariant{
                    name: ident("Lit"),
                    args: vec!(PatternTreeArg{ inner_ty: ident("Lit"), ty: Ty::Alt})
                },
                PatternTreeVariant{
                    name: ident("Array"),
                    args: vec!(PatternTreeArg{ inner_ty: ident("Expr"), ty: Ty::Seq})
                },
                PatternTreeVariant{
                    name: ident("Block"),
                    args: vec!(PatternTreeArg{ inner_ty: ident("Stmt"), ty: Ty::Seq})
                },
                PatternTreeVariant{
                    name: ident("If"),
                    args: vec!(
                        PatternTreeArg { inner_ty: ident("Expr"), ty: Ty::Alt},
                        PatternTreeArg { inner_ty: ident("Stmt"), ty: Ty::Seq},
                        PatternTreeArg { inner_ty: ident("Expr"), ty: Ty::Opt},
                    )
                },
                PatternTreeVariant{
                    name: ident("IfLet"),
                    args: vec!(
                        PatternTreeArg { inner_ty: ident("Stmt"), ty: Ty::Seq},
                        PatternTreeArg { inner_ty: ident("Expr"), ty: Ty::Opt},
                    )
                }
            )
        },
        PatternTreeNode {
            name: ident("Lit"),
            variants: vec!(
                PatternTreeVariant{
                    name: ident("Char"),
                    args: vec!(PatternTreeArg{ inner_ty: ident("char"), ty: Ty::Alt})
                },
                PatternTreeVariant{
                    name: ident("Bool"),
                    args: vec!(PatternTreeArg{ inner_ty: ident("bool"), ty: Ty::Alt})
                },
                PatternTreeVariant{
                    name: ident("Int"),
                    args: vec!(PatternTreeArg{ inner_ty: ident("u128"), ty: Ty::Alt})
                }
            )
        },
        PatternTreeNode {
            name: ident("Stmt"),
            variants: vec!(
                PatternTreeVariant{
                    name: ident("Expr"),
                    args: vec!(PatternTreeArg{ inner_ty: ident("Expr"), ty: Ty::Alt})
                },
                PatternTreeVariant{
                    name: ident("Semi"),
                    args: vec!(PatternTreeArg{ inner_ty: ident("Expr"), ty: Ty::Alt})
                },
            )
        },
    );

    // generate type information struct
    let x = pattern_tree_def.iter().flat_map(|x| &x.variants).map(
        |x| {
            let name = &x.name;
            let args = x.args.iter().map(|x| &x.ty).collect::<Vec<_>>();
            quote!( p.insert(stringify!(#name), vec!(#(Ty::#args),*)); )
        }
    ).collect::<Vec<_>>();
    let types = quote!(
        lazy_static!{
            pub static ref TYPES: std::collections::HashMap<&'static str, Vec<Ty>> = {
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

    // generate enums
    let enums = quote!(
        #(#pattern_tree_def)*
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


#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
