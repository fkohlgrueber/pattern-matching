use quote::quote;
use syn::Ident;
use crate::PatTy;
use std::collections::HashMap;
use common::Ty;


pub(crate) fn gen_result_structs(tmp_name: &Ident, final_name: &Ident, named_subpattern_types: &HashMap<Ident, PatTy>) -> proc_macro2::TokenStream {
    let tmp_struct = gen_tmp_result_struct(tmp_name, named_subpattern_types);
    let final_struct = gen_final_result_struct(tmp_name, final_name, named_subpattern_types);
    quote!(
        #tmp_struct
        #final_struct
    )
}

fn gen_tmp_result_struct(name: &Ident, named_subpattern_types: &HashMap<Ident, PatTy>) -> proc_macro2::TokenStream {
    
    // items of the struct definition
    let struct_def_items = named_subpattern_types.iter().map(
        |(k, v)| {
            let e = &v.inner_ty; 
            match &v.ty {
                Ty::Alt => quote!( #k: Option<&'o A::#e> ),
                Ty::Opt => quote!( #k: Option<&'o A::#e> ),
                Ty::Seq => quote!( #k: Vec<&'o A::#e> ),
            }
        }
    ).collect::<Vec<_>>();

    // items of the struct initialization
    let struct_init_items = named_subpattern_types.iter().map(
        |(k, v)| {
            match &v.ty {
                Ty::Alt => quote!( #k: None ),
                Ty::Opt => quote!( #k: None ),
                Ty::Seq => quote!( #k: vec!() ),
            }
        }
    ).collect::<Vec<_>>();

    // generate struct definition
    let struct_definition = gen_result_struct(name, &struct_def_items, false);
    
    quote!(
        #struct_definition

        // impl new() on the struct
        impl<'o, A> #name<'o, A> 
        where A: pattern::pattern_match::pattern_tree::MatchAssociations<'o> {
            fn new() -> #name<'o, A> {
                #name {
                    #(#struct_init_items),*
                }
            }
        }
    )
}

fn gen_final_result_struct(tmp_name: &Ident, final_name: &Ident, named_subpattern_types: &HashMap<Ident, PatTy>) -> proc_macro2::TokenStream {
    
    // items of the struct definition
    let struct_def_items = named_subpattern_types.iter().map(
        |(k, v)| {
            let e = &v.inner_ty; 
            match &v.ty {
                Ty::Alt => quote!( pub #k: &'o A::#e ),
                Ty::Opt => quote!( pub #k: Option<&'o A::#e> ),
                Ty::Seq => quote!( pub #k: Vec<&'o A::#e> ),
            }
        }
    ).collect::<Vec<_>>();

    // items of the struct initialization (as part of a From impl)
    let struct_init_items = named_subpattern_types.iter().map(
        |(k, v)| {
            match &v.ty {
                Ty::Alt => quote!( #k: cx.#k.unwrap() ),
                Ty::Opt => quote!( #k: cx.#k ),
                Ty::Seq => quote!( #k: cx.#k ),
            }
        }
    ).collect::<Vec<_>>();

    // generate struct definition
    let struct_definition = gen_result_struct(final_name, &struct_def_items, true);
    
    quote!(
        #struct_definition

        // used to convert tmp struct into final struct
        impl<'o, A> From<#tmp_name<'o, A>> for #final_name<'o, A>
        where A: pattern::pattern_match::pattern_tree::MatchAssociations<'o> {
            fn from(cx: #tmp_name<'o, A>) -> Self {
                #final_name {
                    #(#struct_init_items),*
                }
            }
        }

    )
}

fn gen_result_struct(name: &Ident, items: &Vec<proc_macro2::TokenStream>, is_pub: bool) -> proc_macro2::TokenStream {
    let is_pub_tok = if is_pub { quote!( pub ) } else {quote!() };
    quote!(
        #[derive(Debug, Clone)]
        #is_pub_tok struct #name<'o, A>
        where A: pattern::pattern_match::pattern_tree::MatchAssociations<'o> {
            #(#items),*
        }
    )
}