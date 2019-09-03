
use crate::pattern_tree_meta::*;
use crate::{IsMatch, ReduceSelf};

use is_match_macro::derive_is_match_impl;

#[derive(Debug, Clone)]
pub struct MetaParseTree {}

impl<'o> MatchAssociations<'o> for MetaParseTree {
    type ParseTree = pattern_parse::ParseTree;
    type RepeatKind = pattern_parse::RepeatKind;
    type Lit = syn::Lit;
    type LitInt = syn::LitInt;
    type Ident = syn::Ident;
    type LitBool = syn::LitBool;
    type LitChar = syn::LitChar;
}

derive_is_match_impl!{
    ParseTree <> pattern_parse::ParseTree => MetaParseTree => {
        Node(n, e) <> Node(n, e)
        Alt(a, b) <> Alt(a, b)
        Seq(a, b) <> Seq(a, b)
        Repeat_(e, r) <> Repeat(e, r)
        Named(e, s) <> Named(e, s)
        Lit(l) <> Lit(l)
        Any_ <> Any
        Empty <> Empty
    }
}

derive_is_match_impl!{
    RepeatKind <> pattern_parse::RepeatKind => MetaParseTree => {
        Any <> Any
        Plus <> Plus
        Optional <> Optional
        Range(s, e) <> Range(s, e)
        Repeat(r) <> Repeat(r)
    }
}

derive_is_match_impl!{
    Lit <> syn::Lit => MetaParseTree => {
        Bool(b) <> Bool(b)
        Int(n) <> Int(n)
        Char(c) <> Char(c)
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, syn::LitInt> for u128
{
    fn is_match(&self, cx: &'cx mut Cx, other: &syn::LitInt) -> (bool, &'cx mut Cx) {
        match other.base10_parse() {
            Ok(n) => (self == &n, cx),
            Err(_) => (false, cx)
        }
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, syn::LitBool> for bool
{
    fn is_match(&self, cx: &'cx mut Cx, other: &syn::LitBool) -> (bool, &'cx mut Cx) {
        (self == &other.value, cx)
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, syn::Ident> for &'static str
{
    fn is_match(&self, cx: &'cx mut Cx, other: &syn::Ident) -> (bool, &'cx mut Cx) {
        (self == &other.to_string().as_str(), cx)
    }
}

impl<'cx, 'o, Cx: Clone> IsMatch<'cx, 'o, Cx, syn::LitChar> for char
{
    fn is_match(&self, cx: &'cx mut Cx, other: &syn::LitChar) -> (bool, &'cx mut Cx) {
        (self == &other.value(), cx)
    }
}

impl ReduceSelf for syn::LitInt {}
impl ReduceSelf for pattern_parse::ParseTree {}