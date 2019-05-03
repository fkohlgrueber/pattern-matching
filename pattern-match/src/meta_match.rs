
use crate::pattern_tree_meta::*;
use crate::{IsMatch, Reduce, ReduceSelf};

// copied from pattern-macro/parse
use syn;
use syn::Ident;
#[allow(clippy::module_name_repetitions)]
#[derive(PartialEq, Debug, Clone)]
pub enum ParseTree {
    Node(Ident, Vec<ParseTree>),
    Alt(Box<ParseTree>, Box<ParseTree>),
    Seq(Box<ParseTree>, Box<ParseTree>),
    Repeat(Box<ParseTree>, RepeatKind),
    Named(Box<ParseTree>, Ident),
    Lit(syn::Lit),
    Any,
    Empty // matches the empty sequence `()`
}

// copied from pattern-macro/parse
#[derive(PartialEq, Debug, Clone)]
pub enum RepeatKind {
    Any,
    Plus,
    Optional,
    Range(syn::LitInt, Option<syn::LitInt>),
    Repeat(syn::LitInt)
}

#[derive(Debug, Clone)]
pub struct MetaParseTree {}

impl<'o> MatchAssociations<'o> for MetaParseTree {
    type ParseTree = ParseTree;
    type RepeatKind = RepeatKind;
    type Lit = syn::Lit;
}