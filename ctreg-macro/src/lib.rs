extern crate proc_macro;
use proc_macro::TokenStream;

use itertools::Itertools;
use lazy_format::lazy_format;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, quote_each_token, ToTokens};
use regex_syntax::{
    hir::{self, Capture, Class, Hir, HirKind, Literal, Repetition, Visitor},
    parse as parse_regex,
};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, Ident, LitStr, Token,
};
use thiserror::Error;

struct Request {
    name: syn::Ident,
    type_name: syn::Ident,
    regex: syn::LitStr,
}

impl Parse for Request {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let _static: Token![static] = input.parse()?;
        let name = input.parse()?;
        let _colon: Token![:] = input.parse()?;
        let type_name = input.parse()?;
        let _eq: Token![=] = input.parse()?;
        let regex = input.parse()?;

        Ok(Self {
            name,
            type_name,
            regex,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum HirRepState {
    Definite,
    Optional,
    Repeating,
}

impl HirRepState {
    fn from_reps(repetition: &Repetition) -> Self {
        match (repetition.min, repetition.max) {
            (1, Some(1)) => Self::Definite,
            (0, Some(1)) => Self::Optional,
            _ => Self::Repeating,
        }
    }

    fn and(self, other: HirRepState) -> Self {
        Ord::max(self, other)
    }

    fn with(self, repetition: &Repetition) -> Self {
        self.and(Self::from_reps(repetition))
    }
}

#[derive(Debug, Clone, Copy)]
struct GroupInfo<'a> {
    name: &'a str,
    optional: bool,
    index: u32,
}

fn get_group_index(groups: &[GroupInfo<'_>]) -> u32 {
    groups.last().map(|group| group.index).unwrap_or(0) + 1
}

#[derive(Debug, Error)]
enum HirError {
    #[error("duplicate group name: {0:?}")]
    DuplicateGroupName(String),

    #[error("capture group {0:?} is repeating; capture groups can't repeat")]
    RepeatingCaptureGroup(String),
}

fn recurse_hir<'a>(
    hir: &'a Hir,
    groups: &mut Vec<GroupInfo<'a>>,
    state: HirRepState,
) -> Result<Hir, HirError> {
    match *hir.kind() {
        // Literals and their equivelents are passed verbatim
        HirKind::Empty => Ok(Hir::empty()),
        HirKind::Literal(hir::Literal(ref lit)) => Ok(Hir::literal(lit.clone())),
        HirKind::Class(ref class) => Ok(Hir::class(class.clone())),
        HirKind::Look(look) => Ok(Hir::look(look)),

        // Need to compute the repetition state for repetitions
        HirKind::Repetition(ref repetition) => {
            let state = state.with(repetition);
            let sub = recurse_hir(&repetition.sub, groups, state)?;

            Ok(Hir::repetition(Repetition {
                sub: Box::new(sub),
                ..*repetition
            }))
        }
        HirKind::Capture(ref capture) => {
            let Some(name) = capture.name.as_deref() else {
                // Anonymous groups don't capture in ctreg
                return recurse_hir(&capture.sub, groups, state);
            };

            if groups.iter().any(|group| group.name == name) {
                return Err(HirError::DuplicateGroupName(name.to_owned()));
            }

            if state == HirRepState::Repeating {
                return Err(HirError::RepeatingCaptureGroup(name.to_owned()));
            }

            let group_index = get_group_index(groups);

            groups.push(GroupInfo {
                name,
                optional: matches!(state, HirRepState::Optional),
                index: group_index,
            });

            let sub = recurse_hir(&capture.sub, groups, state)?;

            Ok(Hir::capture(Capture {
                index: group_index,
                name: Some(name.into()),
                sub: Box::new(sub),
            }))
        }
        HirKind::Concat(ref concat) => concat
            .iter()
            .map(|sub| recurse_hir(sub, groups, state))
            .try_collect()
            .map(Hir::concat),

        // regex syntax guarantees that alternations have at least 2 variants,
        // so each one is unconditionally optional. In the future we could
        // produce an enum, to reflect that at least one variant will exist
        HirKind::Alternation(ref alt) => alt
            .iter()
            .map(|sub| recurse_hir(sub, groups, state.and(HirRepState::Optional)))
            .try_collect()
            .map(Hir::alternation),
    }
}

macro_rules! push_quote{
    ($tokens:ident, {$($t:tt)*}) => { {quote::quote_each_token!{$tokens $($t)*};} }
}

struct Prefix;

impl ToTokens for Prefix {
    fn to_tokens(&self, mut tokens: &mut TokenStream2) {
        push_quote!(tokens, { ::regex_syntax::hir });
    }
}

struct HirToTokensAdapter<T> {
    item: T,
}

impl ToTokens for HirToTokensAdapter<&Hir> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match *self.item.kind() {
            HirKind::Empty => push_quote!(tokens, {
                #Prefix ::Hir::empty()
            }),
            HirKind::Literal(Literal(ref literal)) => {
                let literal = proc_macro2::Literal::byte_string(literal);

                push_quote!(tokens, {
                    #Prefix ::Hir::literal(#literal)
                })
            }
            HirKind::Class(ref class) => {
                let class = HirToTokensAdapter { item: class };

                push_quote!(tokens, {
                    #Prefix Hir::class(#class)
                })
            }
            HirKind::Look(_) => todo!(),
            HirKind::Repetition(Repetition {
                min,
                max,
                greedy,
                ref sub,
            }) => {
                let sub = HirToTokensAdapter { item: sub.as_ref() };
                let max = HirToTokensAdapter { item: max };

                push_quote!(tokens, {
                    #Prefix ::Hir::repetition(#Prefix ::Repetition {
                        min: #min,
                        max: #max,
                        greedy: #greedy,
                        sub: ::alloc::boxed::Box::new(#sub),
                    })
                })
            }
            HirKind::Capture(Capture {
                index,
                ref name,
                ref sub,
            }) => {}
            HirKind::Concat(_) => todo!(),
            HirKind::Alternation(_) => todo!(),
        }
    }
}

impl ToTokens for HirToTokensAdapter<&Class> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        todo!()
    }
}

impl<T> ToTokens for Option<T>
where
    T: ToTokens,
{
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match *self {
            Some(ref item) => push_quote!(tokens, {
                ::core::Option::Some(#item)
            }),
            None => push_quote!(tokens, { ::core::Option::None }),
        }
    }
}

#[proc_macro]
pub fn regex_impl(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Request);

    let regex = match parse_regex(&input.regex.value()) {
        Ok(regex) => regex,
        Err(err) => {
            return syn::Error::new(
                input.regex.span(),
                lazy_format!("error compiling regex: {err}"),
            )
            .into_compile_error()
            .into()
        }
    };

    todo!()
}
