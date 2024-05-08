/*!
Implementation of the proc macro for `ctreg`. You should never use this crate
directly.
 */

mod render;

extern crate proc_macro;
use proc_macro::TokenStream;

use lazy_format::lazy_format;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use regex_automata::meta::Regex;
use regex_syntax::{
    hir::{self, Capture, Hir, HirKind, Repetition},
    parse as parse_regex,
};
use render::hir_expression;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    spanned::Spanned,
    Ident, Token,
};
use thiserror::Error;

use self::render::{CaptureType, HirType, InputType, RegexType};

struct Request {
    public: Option<Token![pub]>,
    type_name: syn::Ident,
    regex: syn::LitStr,
}

impl Parse for Request {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let public = input.parse()?;
        let type_name = input.parse()?;
        let _eq: Token![=] = input.parse()?;
        let regex = input.parse()?;

        Ok(Self {
            public,
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

    #[error("capture group name {0:?} is not a valid rust identifier")]
    BadName(String),
}

/// Analyze and rewrite the syntax tree
///
/// - Collect information about the capture groups we'll be using
/// - Erase anonymous capture groups
fn process_hir_recurse<'a>(
    hir: &'a Hir,
    groups: &mut Vec<GroupInfo<'a>>,
    state: HirRepState,
) -> Result<Hir, HirError> {
    match *hir.kind() {
        // Literals and their equivalents are passed verbatim
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

        // Capture groups are the most complicated. Need to remove anonymous
        // groups, renumber other groups, and check repetition / optional states.
        HirKind::Capture(ref capture) => {
            let Some(name) = capture.name.as_deref() else {
                // Anonymous groups don't capture in ctreg
                return process_hir_recurse(&capture.sub, groups, state);
            };

            // Let syn do the work for us of validating that this is a correct
            // rust identifier
            let _ident: Ident =
                syn::parse_str(name).map_err(|_| HirError::BadName(name.to_owned()))?;

            // Check duplicate groups
            if groups.iter().any(|group| group.name == name) {
                return Err(HirError::DuplicateGroupName(name.to_owned()));
            }

            // Check repeating groups
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

        // Concatenations are trivial
        HirKind::Concat(ref concat) => concat
            .iter()
            .map(|sub| process_hir_recurse(sub, groups, state))
            .collect::<Result<_, _>>()
            .map(Hir::concat),

        // regex syntax guarantees that alternations have at least 2 variants,
        // so each one is unconditionally optional. In the future we could
        // produce an enum, to reflect that at least one variant will exist
        HirKind::Alternation(ref alt) => alt
            .iter()
            .map(|sub| process_hir_recurse(sub, groups, state.and(HirRepState::Optional)))
            .collect::<Result<_, _>>()
            .map(Hir::alternation),
    }
}

fn process_hir(hir: &Hir) -> Result<(Hir, Vec<GroupInfo<'_>>), HirError> {
    let mut groups = Vec::new();

    process_hir_recurse(hir, &mut groups, HirRepState::Definite).map(|hir| (hir, groups))
}

fn regex_impl_result(input: &Request) -> Result<TokenStream2, syn::Error> {
    let hir = parse_regex(&input.regex.value()).map_err(|error| {
        syn::Error::new(
            input.regex.span(),
            lazy_format!("error compiling regex:\n{error}"),
        )
    })?;

    let (hir, groups) =
        process_hir(&hir).map_err(|error| syn::Error::new(input.regex.span(), error))?;

    // We don't actually use the compiled regex for anything, we just need to
    // ensure that the `hir` does compile correctly.
    let _compiled_regex = Regex::builder().build_from_hir(&hir).map_err(|error| {
        syn::Error::new(
            input.regex.span(),
            lazy_format!("error compiling regex:\n{error}"),
        )
    })?;

    let public = input.public;
    let type_name = &input.type_name;

    let slots_ident = Ident::new("slots", type_name.span());
    let haystack_ident = Ident::new("haystack", type_name.span());

    let mod_name = format_ident!("Mod{type_name}");
    let matches_type_name = format_ident!("{type_name}Captures");

    let matches_fields_definitions = groups.iter().map(|&GroupInfo { name, optional, .. }| {
        let type_name = match optional {
            false => quote! { #CaptureType<'a> },
            true => quote! { ::core::option::Option<#CaptureType<'a>> },
        };

        let field_name = format_ident!("{name}", span = type_name.span());

        quote! { #field_name : #type_name }
    });

    let matches_field_populators = groups.iter().map(
        |&GroupInfo {
             name,
             optional,
             index,
         }| {
            let slot_start = (index as usize) * 2;
            let slot_end = slot_start + 1;

            let field_name = format_ident!("{name}", span = type_name.span());

            let populate = quote! {{
                let slot_start = #slots_ident[#slot_start];
                let slot_end = #slots_ident[#slot_end];

                match slot_start {
                    None => None,
                    Some(start) => {
                        let start = start.get();
                        let end = unsafe { slot_end.unwrap_unchecked() }.get();
                        let content = unsafe { #haystack_ident.get_unchecked(start..end) };

                        Some(#CaptureType {start, end, content})
                    }
                }
            }};

            let expr = match optional {
                true => populate,
                false => quote! {
                    match #populate {
                        Some(capture) => capture,
                        None => unsafe { ::core::hint::unreachable_unchecked() },
                    }
                },
            };

            quote! { #field_name : #expr }
        },
    );

    let num_capture_groups = groups.len();

    let captures_impl = (num_capture_groups > 0).then(|| quote! {
        impl #type_name {
            #[inline]
            #[must_use]
            pub fn captures<'i>(&self, #haystack_ident: &'i str) -> ::core::option::Option<#matches_type_name<'i>> {
                let mut #slots_ident = [::core::option::Option::None; (#num_capture_groups + 1) * 2];
                let _ = self.regex.search_slots(&#InputType::new(#haystack_ident), &mut #slots_ident)?;

                ::core::option::Option::Some(#matches_type_name {
                    #(#matches_field_populators ,)*
                })
            }
        }

        #[derive(Debug, Clone, Copy)]
        pub struct #matches_type_name<'a> {
            #(pub #matches_fields_definitions,)*
        }
    });

    let captures_export = captures_impl.is_some().then(|| {
        quote! {
            #public use #mod_name::#matches_type_name
        }
    });

    let rendered_hir = hir_expression(&hir);

    Ok(quote! {
        #[doc(hidden)]
        #[allow(non_snake_case)]
        mod #mod_name {
            #[derive(Debug, Clone)]
            pub struct #type_name {
                regex: #RegexType,
            }

            impl #type_name {
                #[inline]
                #[must_use]
                pub fn new() -> Self {
                    let hir: #HirType = #rendered_hir;
                    let regex = #RegexType::builder()
                        .build_from_hir(&hir)
                        .expect("regex compilation failed, despite compile-time verification");
                    Self { regex }
                }

                #[inline]
                #[must_use]
                pub fn is_match(&self, haystack: &str) -> bool {
                    self.regex.is_match(haystack)
                }

                #[inline]
                #[must_use]
                pub fn find<'i>(&self, haystack: &'i str) -> ::core::option::Option<#CaptureType<'i>> {
                    let capture = self.regex.find(haystack)?;
                    let span = capture.span();

                    let start = span.start;
                    let end = span.end;
                    let content = unsafe { haystack.get_unchecked(start..end) };

                    Some(#CaptureType { start, end, content })
                }
            }

            impl ::core::default::Default for #type_name {
                fn default() -> Self {
                    Self::new()
                }
            }

            #captures_impl
        }

        #public use #mod_name::#type_name;
        #captures_export;

    })
}

#[proc_macro]
pub fn regex_impl(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Request);

    regex_impl_result(&input)
        .unwrap_or_else(|error| error.into_compile_error())
        .into()
}
