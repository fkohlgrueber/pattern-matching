extern crate proc_macro;
use quote::quote;
use proc_macro::TokenStream;
use common::Ty;
use syn::parse::{Parse, ParseStream};
use syn::{Result, token, Token, parenthesized, punctuated::Punctuated};

struct PatternTree(Vec<PatternTreeNode>);

#[derive(Clone, Debug)]
struct PatternTreeNode {
    name: syn::Ident,
    variants: Vec<PatternTreeVariant>
}

#[derive(Clone, Debug)]
struct PatternTreeVariant {
    name: syn::Ident,
    args: Vec<PatternTreeArg>
}

#[derive(Clone, Debug)]
struct PatternTreeArg {
    ty: Ty,
    assoc_ty: syn::Ident,
    custom_assoc_ty: Option<syn::Ident>
}

impl PatternTreeNode {
    fn uses_assoc_types(&self, cx: &std::collections::HashMap<String, PatternTreeNode>) -> bool {
        // if any argument contains a type that's not a terminal type, the node needs to have the generic parameter `A`.
        for variant in &self.variants {
            for arg in &variant.args {
                if cx.contains_key(&arg.assoc_ty.to_string()) || arg.custom_assoc_ty.is_some() {
                    return true;
                }
            }
        }
        false
    }

    fn has_args(&self) -> bool {
        // if any of the node's variants has an argument, the node needs to have the generic parameters `'cx`, `'o` and `Cx`.
        for variant in &self.variants {
            if !variant.args.is_empty() {
                return true;
            }
        }
        false
    }
}

impl Parse for PatternTree {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut nodes = vec!();
        while !input.is_empty() {
            nodes.push(input.parse()?)
        }
        Ok(
            PatternTree(nodes)
        )
    }
}

impl Parse for PatternTreeNode {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse()?;
        input.parse::<Token![=]>()?;
        
        // parse variants
        let mut variants = vec!(input.parse()?);
        while input.peek(Token![|]) {
            input.parse::<Token![|]>()?;
            variants.push(input.parse()?);
        }
        Ok(
            PatternTreeNode {
                name,
                variants
            }
        )
    }
}

impl Parse for PatternTreeVariant {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse()?;
        let args = if input.peek(token::Paren) {
            let content;
            parenthesized!(content in input);
            let vals: Punctuated<PatternTreeArg, Token![,]>;
            vals = content.parse_terminated(PatternTreeArg::parse)?;
            vals.into_iter().collect()
        } else {
            vec!()
        };
        
        Ok(
            PatternTreeVariant {
                name,
                args
            }
        )
    }
}

impl Parse for PatternTreeArg {
    fn parse(input: ParseStream) -> Result<Self> {
        let assoc_ty = input.parse()?;
        
        // parse repetition
        let ty = if input.peek(Token![*]) {
            input.parse::<Token![*]>()?;
            Ty::Seq
        } else if input.peek(Token![?]) {
            input.parse::<Token![?]>()?;
            Ty::Opt
        } else {
            Ty::Alt
        };

        // parse custom associated type name
        let custom_assoc_ty = if input.peek(Token![<]) {
            input.parse::<Token![<]>()?;
            input.parse::<Token![>]>()?;
            Some(input.parse()?)
        } else {
            None
        };
        
        Ok(
            PatternTreeArg {
                ty,
                assoc_ty,
                custom_assoc_ty
            }
        )
    }
}


#[proc_macro]
pub fn pattern_tree(input: TokenStream) -> TokenStream {
    // parse input
    let pattern_tree_def = syn::parse_macro_input!(input as PatternTree).0;

    // use hashmap for lookups
    let mut pt_hashmap = std::collections::HashMap::new();
    for node in pattern_tree_def.iter() {
        pt_hashmap.insert(node.name.to_string(), node.clone());
    }
    
    let enums = pattern_tree_def.iter().map(|x| pt_node_to_tokens(&pt_hashmap, x)).collect::<Vec<_>>();

    // generate type information struct
    let items = pattern_tree_def.iter().flat_map(|x| &x.variants).map(
        |x| {
            let name = &x.name;
            let args = x.args.iter().map(|x| &x.ty).collect::<Vec<_>>();
            let args_id = x.args.iter().map(|x| 
                match &x.custom_assoc_ty {
                    Some(ty) => &ty,
                    None => &x.assoc_ty
                }
            ).collect::<Vec<_>>();
            quote!( p.insert(stringify!(#name), vec!(#( (stringify!(#args_id), pattern_tree::Ty::#args) ),*)); )
        }
    ).collect::<Vec<_>>();
    let types = quote!(
        pattern_tree::lazy_static!{
            pub static ref TYPES: rustc_data_structures::fx::FxHashMap<&'static str, Vec<(&'static str, pattern_tree::Ty)>> = {
                let mut p = rustc_data_structures::fx::FxHashMap::default();
                #(#items)*
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

    // generate MatchAssociations Struct
    let mut match_associations_struct = pattern_tree_def.iter().map(|x| {
        let name = &x.name;
        quote!(
            type #name: 'o + std::fmt::Debug + Clone;
        )
    }).collect::<Vec<_>>();

    let custom_assoc_types = pattern_tree_def.iter()
        .flat_map(|x| &x.variants)
        .flat_map(|x| &x.args)
        .filter_map(|x| x.custom_assoc_ty.clone())
        .collect::<std::collections::HashSet<_>>();
    for ident in custom_assoc_types {
        match_associations_struct.push(
            quote!(
                type #ident: 'o + std::fmt::Debug + Clone;
            )
        );
    }

    let tokens = 
    quote!(
        #(#enums)*
        #types
        #exports

        pub trait MatchAssociations<'o> 
        where Self: Sized + Clone {
            #(#match_associations_struct)*
        }
    );

    //println!("{}\n\n\n", tokens.to_string());
    tokens.into()
}

fn pt_node_to_tokens(cx: &std::collections::HashMap<String, PatternTreeNode>, node: &PatternTreeNode) -> proc_macro2::TokenStream {
    let name = &node.name;
    let assoc = node.uses_assoc_types(cx);
    let has_args = node.has_args();
    
    let generic_params = if assoc && has_args {
        quote!( <'cx, 'o, Cx, A> )
    } else if has_args {
        quote!( <'cx, 'o, Cx> )
    } else {
        quote!()
    };

    let where_clause = if assoc {
        quote!( where A: MatchAssociations<'o> )
    } else {
        quote!()
    };

    let variants = node.variants.iter().map(|x| pt_variant_to_tokens(cx, x)).collect::<Vec<_>>();
    
    quote!(
        #[derive(Debug)]
        pub enum #name #generic_params
        #where_clause
        
        {
            #(#variants),*
        }

        // implementation of the PatternTreeNode trait
        impl #generic_params ::pattern_tree::PatternTreeNode for #name #generic_params #where_clause {}
    )
}

fn pt_variant_to_tokens(cx: &std::collections::HashMap<String, PatternTreeNode>, variant: &PatternTreeVariant) -> proc_macro2::TokenStream {
    let name = &variant.name;
    let arg_tokens = if variant.args.is_empty() {
        quote!()
    } else {
        let args = variant.args.iter().map(|x| pt_arg_to_tokens(cx, x)).collect::<Vec<_>>();
        quote!( ( #(#args),* ) )
    };
    quote!(#name #arg_tokens)
}

fn pt_arg_to_tokens(cx: &std::collections::HashMap<String, PatternTreeNode>, arg: &PatternTreeArg) -> proc_macro2::TokenStream {
    match cx.get(&arg.assoc_ty.to_string()) {
        Some(e) => {
            let assoc = e.uses_assoc_types(cx);
            let has_args = e.has_args();
            let repeat = &arg.ty;
            let assoc_ty = &arg.assoc_ty;
            let custom_assoc_ty = match &arg.custom_assoc_ty {
                Some(e) => &e,
                None => &arg.assoc_ty,
            };
            if assoc && has_args {
                quote!(::pattern_tree::matchers::#repeat<'cx, 'o, #assoc_ty<'cx, 'o, Cx, A>, Cx, A::#custom_assoc_ty>)
            } else if has_args {
                quote!(::pattern_tree::matchers::#repeat<'cx, 'o, #assoc_ty<'cx, 'o, Cx>, Cx, A::#custom_assoc_ty>)
            } else {
                quote!(::pattern_tree::matchers::#repeat<'cx, 'o, #assoc_ty, Cx, A::#custom_assoc_ty>)
            }
        }
        None => {
            let repeat = &arg.ty;
            let assoc_ty = &arg.assoc_ty;
            let custom_assoc_ty = match &arg.custom_assoc_ty {
                Some(e) => quote!(A::#e),
                None => quote!(#assoc_ty),
            };
            quote!(::pattern_tree::matchers::#repeat<'cx, 'o, #assoc_ty, Cx, #custom_assoc_ty>)
        }
    }
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