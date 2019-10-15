//! This crate contains the `gen_pattern_macro` procedural macro. This crate is
//! not supposed to be used directly. use the `pattern-macro-gen` crate instead.
#![recursion_limit="512"]
extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{Result, Token};
use syn::parse::{Parse, ParseStream};
use syn;


struct GenPatternMacro {
    name: syn::Ident,
    module: syn::Path
}

impl Parse for GenPatternMacro {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse()?;
        input.parse::<Token![=>]>()?;
        let module = input.parse()?;
        Ok(
            GenPatternMacro {
                name,
                module
            }
        )
    }
}


#[proc_macro]
pub fn gen_pattern_macro(item: TokenStream) -> TokenStream {
    let hash = proc_macro2::TokenTree::Punct(proc_macro2::Punct::new('#', proc_macro2::Spacing::Alone));

    let GenPatternMacro { name, module } = syn::parse_macro_input!(item as GenPatternMacro);

    quote!(
        #[proc_macro]
        pub fn #name(item: TokenStream) -> TokenStream {

            //println!("---------------\n{}", item);

            let pattern_name = pattern_macro_gen::proc_macro2::Ident::new(stringify!(#name), pattern_macro_gen::proc_macro2::Span::call_site());

            let item_orig = pattern_macro_gen::proc_macro2::TokenStream::from(item.clone());
            // parse the pattern
            let pattern_macro_gen::Pattern { name, ty, repeat_ty, node } = pattern_macro_gen::syn::parse_macro_input!(item as pattern_macro_gen::Pattern);

            // if the pattern contains unresolved pattern functions, expand those
            if let Some(s) = pattern_macro_gen::needs_expansion(&node) {
                let ident = pattern_macro_gen::proc_macro2::Ident::new(&s, pattern_macro_gen::proc_macro2::Span::call_site());
                return pattern_macro_gen::quote!(
                    #hash ident !{
                        #hash pattern_name 
                        #hash item_orig
                    }
                ).into()
            }

            // wrap parsed pattern with named `root` so that the pattern result struct has at least one item
            let node = pattern_macro_gen::ParseTree::Named(Box::new(node), pattern_macro_gen::proc_macro2::Ident::new("root", pattern_macro_gen::proc_macro2::Span::call_site()));
            
            // name of the result struct is <pattern_name>Struct, e.g. PatStruct
            let struct_name = pattern_macro_gen::proc_macro2::Ident::new(&(name.to_string() + "Struct"), pattern_macro_gen::proc_macro2::Span::call_site());
            
            // name of the temporary result struct is <pattern_name>TmpStruct, e.g. PatTmpStruct
            // this struct is used as the context during matching
            let struct_tmp_name = pattern_macro_gen::proc_macro2::Ident::new(&(name.to_string() + "TmpStruct"), pattern_macro_gen::proc_macro2::Span::call_site());

            // for each named subpattern, get its type
            let named_subpattern_types = match pattern_macro_gen::get_named_subpattern_types(&node, &ty, &#module::TYPES) {
                Ok(t) => t,
                Err(ts) => return ts.to_compile_error().into(),
            };


            // generate the actual pattern structure
            let tokens = pattern_macro_gen::to_tokens(&node, repeat_ty, &named_subpattern_types, &#module::TYPES, &pattern_macro_gen::quote!(#module));

            // generate result structs (and their impls)
            let result_structs = pattern_macro_gen::gen_result_structs(&struct_tmp_name, &struct_name, &named_subpattern_types, &pattern_macro_gen::quote!(#module));

            let tokens = pattern_macro_gen::quote!(
                // result structs
                #hash result_structs
                
                // matching function
                fn #hash name <'o, A, P> (node: &'o P) -> Option<#hash struct_name<'o, A>> 
                where 
                    A: pattern::#module::MatchAssociations<'o, #hash ty=P>,
                    P: std::fmt::Debug + std::clone::Clone,
                    for<'cx> pattern::#module::#hash ty<'cx, 'o, #hash struct_tmp_name<'o, A>, A>: pattern::pattern_match::IsMatch<
                        'cx, 
                        'o, 
                        #hash struct_tmp_name<'o, A>, 
                        P
                    >,
                {
                    use pattern::pattern_match::IsMatch;

                    // initialize the pattern
                    let pattern: pattern::matchers::#hash repeat_ty<
                        '_, 
                        '_, 
                        pattern::#module::#hash ty<
                            '_, 
                            '_, 
                            #hash struct_tmp_name<A>, 
                            A
                        >, 
                        #hash struct_tmp_name<A>, 
                        A::#hash ty
                    > = #hash tokens;

                    // initialize the result struct
                    let mut cx = #hash struct_tmp_name::new();

                    // match input node against pattern
                    let (r, cx_out) = pattern.is_match(&mut cx, node);
                    
                    if r {
                        // convert cx to final struct and return
                        Some(cx.into())
                    } else {
                        None
                    }
                }
            );
            //println!("{}\n\n\n", tokens.clone().to_string());
            tokens.into()
        }
    ).into()
}

