use quote::quote;
use syn;
use syn::Ident;
use crate::PatTy;
use std::collections::HashMap;
use common::Ty;


pub fn gen_result_structs(
    tmp_name: &Ident, 
    final_name: &Ident, 
    named_subpattern_types: &HashMap<Ident, PatTy>,
    module: &proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let tmp_struct = gen_tmp_result_struct(tmp_name, named_subpattern_types, module);
    let final_struct = gen_final_result_struct(tmp_name, final_name, named_subpattern_types, module);
    quote!(
        #tmp_struct
        #final_struct
    )
}

fn gen_tmp_result_struct(
    name: &Ident, 
    named_subpattern_types: &HashMap<Ident, PatTy>,
    module: &proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    
    // items of the struct definition
    let struct_def_items = named_subpattern_types.iter().map(
        |(k, v)| {
            let e = quote_type(&v.inner_ty); 
            match &v.ty {
                Ty::Alt |
                Ty::Opt => quote!( #k: Option<&'o #e> ),
                Ty::Seq => quote!( #k: Vec<&'o #e> ),
            }
        }
    ).collect::<Vec<_>>();

    // items of the struct initialization
    let struct_init_items = named_subpattern_types.iter().map(
        |(k, v)| {
            match &v.ty {
                Ty::Alt |
                Ty::Opt => quote!( #k: None ),
                Ty::Seq => quote!( #k: vec!() ),
            }
        }
    ).collect::<Vec<_>>();

    // generate struct definition
    let struct_definition = gen_result_struct(name, &struct_def_items, false, module);
    
    quote!(
        #struct_definition

        // impl new() on the struct
        impl<'o, A> #name<'o, A> 
        where A: pattern::#module::MatchAssociations<'o> {
            fn new() -> #name<'o, A> {
                #name {
                    #(#struct_init_items),*
                }
            }
        }
    )
}

fn gen_final_result_struct(
    tmp_name: &Ident, 
    final_name: &Ident, 
    named_subpattern_types: &HashMap<Ident, PatTy>,
    module: &proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    
    // items of the struct definition
    let struct_def_items = named_subpattern_types.iter().map(
        |(k, v)| {
            let e = quote_type(&v.inner_ty); 
            match &v.ty {
                Ty::Alt => quote!( pub #k: &'o #e ),
                Ty::Opt => quote!( pub #k: Option<&'o #e> ),
                Ty::Seq => quote!( pub #k: Vec<&'o #e> ),
            }
        }
    ).collect::<Vec<_>>();

    // items of the struct initialization (as part of a From impl)
    let struct_init_items = named_subpattern_types.iter().map(
        |(k, v)| {
            match &v.ty {
                Ty::Alt => quote!( #k: cx.#k.unwrap() ),
                Ty::Opt |
                Ty::Seq => quote!( #k: cx.#k ),
            }
        }
    ).collect::<Vec<_>>();

    // generate struct definition
    let struct_definition = gen_result_struct(final_name, &struct_def_items, true, module);
    
    quote!(
        #struct_definition

        // used to convert tmp struct into final struct
        impl<'o, A> From<#tmp_name<'o, A>> for #final_name<'o, A>
        where A: pattern::#module::MatchAssociations<'o> {
            fn from(cx: #tmp_name<'o, A>) -> Self {
                #final_name {
                    #(#struct_init_items),*
                }
            }
        }

    )
}

fn gen_result_struct(
    name: &Ident, 
    items: &[proc_macro2::TokenStream], 
    is_pub: bool, 
    module: &proc_macro2::TokenStream
) -> proc_macro2::TokenStream {
    let is_pub_tok = if is_pub { quote!( pub ) } else {quote!() };
    quote!(
        #[derive(Debug, Clone)]
        #is_pub_tok struct #name<'o, A>
        where A: pattern::#module::MatchAssociations<'o> {
            #(#items),*
        }
    )
}

fn quote_type(e: &proc_macro2::Ident) -> proc_macro2::TokenStream {
    // Add `A::` prefix to all types except the terminal ones
    if ["bool", "char", "u128"].contains(&e.to_string().as_str()) {
        quote!(#e)
    } else {
        quote!(A::#e)
    }
}