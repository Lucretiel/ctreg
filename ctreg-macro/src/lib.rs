extern crate proc_macro;
use proc_macro::TokenStream;

use itertools::Itertools;
use lazy_format::lazy_format;
use proc_macro2::{Literal as LiteralToken, TokenStream as TokenStream2};
use quote::{quote, quote_each_token, ToTokens};
use regex_syntax::{
    hir::{
        self, Capture, Class, ClassUnicode, ClassUnicodeRange, Hir, HirKind, Literal, Look,
        Repetition, Visitor,
    },
    parse as parse_regex,
};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, Ident, LitStr, Token,
};
use thiserror::Error;

struct Request {
    public: bool,
    name: syn::Ident,
    type_name: syn::Ident,
    regex: syn::LitStr,
}

impl Parse for Request {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let public: Option<Token![pub]> = input.parse()?;
        let _static: Token![static] = input.parse()?;
        let name = input.parse()?;
        let _colon: Token![:] = input.parse()?;
        let type_name = input.parse()?;
        let _eq: Token![=] = input.parse()?;
        let regex = input.parse()?;

        Ok(Self {
            public: public.is_some(),
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

fn process_hir_recurse<'a>(
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
            let sub = process_hir_recurse(&repetition.sub, groups, state)?;

            Ok(Hir::repetition(Repetition {
                sub: Box::new(sub),
                ..*repetition
            }))
        }
        HirKind::Capture(ref capture) => {
            let Some(name) = capture.name.as_deref() else {
                // Anonymous groups don't capture in ctreg
                return process_hir_recurse(&capture.sub, groups, state);
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

            let sub = process_hir_recurse(&capture.sub, groups, state)?;

            Ok(Hir::capture(Capture {
                index: group_index,
                name: Some(name.into()),
                sub: Box::new(sub),
            }))
        }
        HirKind::Concat(ref concat) => concat
            .iter()
            .map(|sub| process_hir_recurse(sub, groups, state))
            .try_collect()
            .map(Hir::concat),

        // regex syntax guarantees that alternations have at least 2 variants,
        // so each one is unconditionally optional. In the future we could
        // produce an enum, to reflect that at least one variant will exist
        HirKind::Alternation(ref alt) => alt
            .iter()
            .map(|sub| process_hir_recurse(sub, groups, state.and(HirRepState::Optional)))
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
        push_quote!(tokens, { ::ctreg::private::regex_automata::hir });
    }
}

fn render_option<T: ToTokens>(opt: Option<T>) -> TokenStream2 {
    match opt {
        Some(item) => quote! { ::core::Option::Some(#item) },
        None => quote! { ::core::Option::None },
    }
}

fn render_class(class: &Class) -> TokenStream2 {
    match *class {
        Class::Unicode(ref class) => {
            let class = class.ranges().iter().map(|range| {
                let start = range.start();
                let end = range.end();

                quote! { #Prefix ::ClassUnicodeRange::new(#start, #end) }
            });

            quote! {
                #Prefix ::Class::Unicode(#Prefix ::ClassUnicode::new([#(#class,)*]))
            }
        }
        Class::Bytes(ref class) => {
            let class = class.ranges().iter().map(|range| {
                let start = range.start();
                let end = range.end();

                quote! { #Prefix ::ClassBytesRange::new(#start, #end) }
            });

            quote! {
                #Prefix ::Class::Bytes(#Prefix ::ClassBytes::new([#(#class,)*]))
            }
        }
    }
}

macro_rules! render_look {
    ($($Variant:ident)*) => {
        fn render_look(look: Look) -> TokenStream2 {
            match look {$(
                Look::$Variant => quote! { #Prefix ::Look::$Variant },
            )*}
        }
    }
}

render_look! {
    Start
    End
    StartLF
    EndLF
    StartCRLF
    EndCRLF
    WordAscii
    WordAsciiNegate
    WordUnicode
    WordUnicodeNegate
    WordStartAscii
    WordEndAscii
    WordStartUnicode
    WordEndUnicode
    WordStartHalfAscii
    WordEndHalfAscii
    WordStartHalfUnicode
    WordEndHalfUnicode
}

fn render_hir(hir: &Hir) -> TokenStream2 {
    match *hir.kind() {
        HirKind::Empty => {
            quote! { #Prefix ::Hir::empty() }
        }
        HirKind::Literal(Literal(ref literal)) => {
            let literal = LiteralToken::byte_string(literal);

            quote! { #Prefix ::Hir::literal(#literal) }
        }
        HirKind::Class(ref class) => {
            let class = render_class(class);

            quote! { #Prefix ::Hir::class(#class) }
        }
        HirKind::Look(look) => {
            let look = render_look(look);

            quote! { #Prefix ::Hir::look(#look) }
        }
        HirKind::Repetition(Repetition {
            min,
            max,
            greedy,
            ref sub,
        }) => {
            let max = render_option(max.as_ref());
            let sub = render_hir(sub);

            quote! {
                #Prefix ::Hir::repetition(#Prefix ::Repetition {
                    min: #min,
                    max: #max,
                    greedy: #greedy,
                    sub: #sub,
                })
            }
        }
        HirKind::Capture(Capture {
            index,
            ref name,
            ref sub,
        }) => {
            let name = render_option(
                name.as_deref()
                    .map(|name| quote!({ ::alloc::boxed::Box::from(#name) })),
            );

            let sub = render_hir(sub);

            quote! {
                #Prefix ::Hir::capture(#Prefix ::Capture {
                    index: #index,
                    name: #name,
                    sub: #sub,
                })
            }
        }
        HirKind::Concat(ref concat) => {
            let concat = concat.iter().map(render_hir);

            quote! {
                #Prefix ::Hir::concat(::alloc::Vec::from([#(#concat,)*]))
            }
        }
        HirKind::Alternation(ref alternation) => {
            let alternation = alternation.iter().map(render_hir);

            quote! {
                #Prefix ::Hir::alternation(::alloc::Vec::from([#(#alternation,)*]))
            }
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
