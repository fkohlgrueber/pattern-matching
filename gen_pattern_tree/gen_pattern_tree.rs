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
                    args: vec!(PatternTreeArg{ inner_ty: ident("Lit"), ty: Ty::Alt, assoc_ty: ident("Lit"), inner_ty_primitive: false})
                },
                PatternTreeVariant{
                    name: ident("Array"),
                    args: vec!(PatternTreeArg{ inner_ty: ident("Expr"), ty: Ty::Seq, assoc_ty: ident("Expr"), inner_ty_primitive: false})
                },
                PatternTreeVariant{
                    name: ident("Block_"),
                    args: vec!(PatternTreeArg{ inner_ty: ident("BlockType"), ty: Ty::Alt, assoc_ty: ident("BlockType"), inner_ty_primitive: false})
                },
                PatternTreeVariant{
                    name: ident("If"),
                    args: vec!(
                        PatternTreeArg { inner_ty: ident("Expr"), ty: Ty::Alt, assoc_ty: ident("Expr"), inner_ty_primitive: false},
                        PatternTreeArg { inner_ty: ident("BlockType"), ty: Ty::Alt, assoc_ty: ident("BlockType"), inner_ty_primitive: false},
                        PatternTreeArg { inner_ty: ident("Expr"), ty: Ty::Opt, assoc_ty: ident("Expr"), inner_ty_primitive: false},
                    )
                },
                PatternTreeVariant{
                    name: ident("IfLet"),
                    args: vec!(
                        PatternTreeArg { inner_ty: ident("BlockType"), ty: Ty::Alt, assoc_ty: ident("BlockType"), inner_ty_primitive: false},
                        PatternTreeArg { inner_ty: ident("Expr"), ty: Ty::Opt, assoc_ty: ident("Expr"), inner_ty_primitive: false},
                    )
                }
            )
        },
        PatternTreeNode {
            name: ident("Lit"),
            variants: vec!(
                PatternTreeVariant{
                    name: ident("Char"),
                    args: vec!(PatternTreeArg{ inner_ty: ident("char"), ty: Ty::Alt, assoc_ty: ident("Char"), inner_ty_primitive: true})
                },
                PatternTreeVariant{
                    name: ident("Bool"),
                    args: vec!(PatternTreeArg{ inner_ty: ident("bool"), ty: Ty::Alt, assoc_ty: ident("Bool"), inner_ty_primitive: true})
                },
                PatternTreeVariant{
                    name: ident("Int"),
                    args: vec!(PatternTreeArg{ inner_ty: ident("u128"), ty: Ty::Alt, assoc_ty: ident("Int"), inner_ty_primitive: true})
                }
            )
        },
        PatternTreeNode {
            name: ident("Stmt"),
            variants: vec!(
                PatternTreeVariant{
                    name: ident("Expr"),
                    args: vec!(PatternTreeArg{ inner_ty: ident("Expr"), ty: Ty::Alt, assoc_ty: ident("Expr"), inner_ty_primitive: false})
                },
                PatternTreeVariant{
                    name: ident("Semi"),
                    args: vec!(PatternTreeArg{ inner_ty: ident("Expr"), ty: Ty::Alt, assoc_ty: ident("Expr"), inner_ty_primitive: false})
                },
            )
        },
        PatternTreeNode {
            name: ident("BlockType"),
            variants: vec!(
                PatternTreeVariant{
                    name: ident("Block"),
                    args: vec!(PatternTreeArg{ inner_ty: ident("Stmt"), ty: Ty::Seq, assoc_ty: ident("Stmt"), inner_ty_primitive: false})
                },
            )
        },
    );

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
