#[proc_macro]
pub fn pattern(item: TokenStream) -> TokenStream {
    let pattern_name = pattern_macro_gen::proc_macro2::Ident::new(
        stringify!(pattern),
        pattern_macro_gen::proc_macro2::Span::call_site(),
    );
    let item_orig = pattern_macro_gen::proc_macro2::TokenStream::from(item.clone());
    let pattern_macro_gen::Pattern {
        name,
        ty,
        repeat_ty,
        node,
    } = pattern_macro_gen::syn::parse_macro_input!(item as pattern_macro_gen::Pattern);
    if let Some(s) = pattern_macro_gen::needs_expansion(&node) {
        let ident = pattern_macro_gen::proc_macro2::Ident::new(
            &s,
            pattern_macro_gen::proc_macro2::Span::call_site(),
        );
        let tokens = pattern_macro_gen :: quote ! (
# ident ! { # pattern_name # item_orig } );
        return tokens.into();
    }
    let node = pattern_macro_gen::ParseTree::Named(
        Box::new(node),
        pattern_macro_gen::proc_macro2::Ident::new(
            "root",
            pattern_macro_gen::proc_macro2::Span::call_site(),
        ),
    );
    let struct_name = pattern_macro_gen::proc_macro2::Ident::new(
        &(name.to_string() + "Struct"),
        pattern_macro_gen::proc_macro2::Span::call_site(),
    );
    let struct_tmp_name = pattern_macro_gen::proc_macro2::Ident::new(
        &(name.to_string() + "TmpStruct"),
        pattern_macro_gen::proc_macro2::Span::call_site(),
    );
    let named_subpattern_types = match pattern_macro_gen::get_named_subpattern_types(
        &node,
        &ty,
        &pattern_match::pattern_tree_rust::TYPES,
    ) {
        Ok(t) => t,
        Err(ts) => return ts.to_compile_error().into(),
    };
    let tokens = pattern_macro_gen::to_tokens(
        &node,
        repeat_ty,
        &named_subpattern_types,
        &pattern_match::pattern_tree_rust::TYPES,
        &pattern_macro_gen::quote!(pattern_match::pattern_tree_rust),
    );
    let result_structs = pattern_macro_gen::gen_result_structs(
        &struct_tmp_name,
        &struct_name,
        &named_subpattern_types,
        &pattern_macro_gen::quote!(pattern_match::pattern_tree_rust),
    );
    let tokens = pattern_macro_gen :: quote ! (
# result_structs fn # name < 'o , A , P > ( node : & 'o P ) -> Option < #
struct_name < 'o , A >> where A : pattern :: pattern_match ::
pattern_tree_rust :: MatchAssociations < 'o , # ty = P > , P : std :: fmt ::
Debug + std :: clone :: Clone , for < 'cx > pattern :: pattern_match ::
pattern_tree_rust :: # ty < 'cx , 'o , # struct_tmp_name < 'o , A > , A > :
pattern :: pattern_match :: IsMatch < 'cx , 'o , # struct_tmp_name < 'o , A >
, P > , {
use pattern :: pattern_match :: IsMatch ; let pattern : pattern :: matchers ::
# repeat_ty < '_ , '_ , pattern :: pattern_match :: pattern_tree_rust :: # ty
< '_ , '_ , # struct_tmp_name < A > , A > , # struct_tmp_name < A > , A :: #
ty > = # tokens ; let mut cx = # struct_tmp_name :: new (  ) ; let (
r , cx_out ) = pattern . is_match ( & mut cx , node ) ; if r {
Some ( cx . into (  ) ) } else { None } } );
    tokens.into()
}
