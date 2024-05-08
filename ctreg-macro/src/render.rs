use proc_macro2::{Literal as LiteralToken, TokenStream as TokenStream2};
use quote::{quote, ToTokens};
use regex_syntax::hir::{Capture, Class, Hir, HirKind, Literal, Look, Repetition};

macro_rules! quote_push {
    ($tokens:ident, { $($t:tt)* }) => { { quote::quote_each_token! { $tokens $($t)* }; } };
}

macro_rules! prefixes {
    ($(
         $Name:ident = {$($t:tt)*}
    )*) => {$(
        pub struct $Name;

        impl ToTokens for $Name {
            fn to_tokens(&self, mut tokens: &mut TokenStream2) {
                quote_push!(tokens, {$($t)*})
            }
        }
    )*}
}

prefixes! {
    CaptureType = { ::ctreg::Capture }
    Private = { ::ctreg::ඞ }

    AutomataMod = { #Private::regex_automata }
    RegexType = { #AutomataMod::meta::Regex }
    InputType = { #AutomataMod::Input }

    HirMod = { #Private::regex_syntax::hir }
    HirType = { #HirMod::Hir }
}

#[inline]
#[must_use]
fn render_class(class: &Class) -> TokenStream2 {
    match *class {
        Class::Unicode(ref class) => {
            let class = class.ranges().iter().map(|range| {
                let start = range.start();
                let end = range.end();

                quote! { #HirMod ::ClassUnicodeRange::new(#start, #end) }
            });

            quote! {
                #HirMod ::Class::Unicode(#HirMod ::ClassUnicode::new([#(#class,)*]))
            }
        }
        Class::Bytes(ref class) => {
            let class = class.ranges().iter().map(|range| {
                let start = range.start();
                let end = range.end();

                quote! { #HirMod ::ClassBytesRange::new(#start, #end) }
            });

            quote! {
                #HirMod ::Class::Bytes(#HirMod ::ClassBytes::new([#(#class,)*]))
            }
        }
    }
}

macro_rules! render_look {
    ($($Variant:ident)*) => {
        #[inline]
        #[must_use]
        fn render_look(look: Look) -> TokenStream2 {
            match look {$(
                Look::$Variant => quote! { #HirMod ::Look::$Variant },
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

#[inline]
#[must_use]
pub fn hir_expression(hir: &Hir) -> TokenStream2 {
    match *hir.kind() {
        HirKind::Empty => {
            quote! { #HirType::empty() }
        }
        HirKind::Literal(Literal(ref literal)) => {
            let literal = LiteralToken::byte_string(literal);

            quote! { #HirType::literal(*#literal) }
        }
        HirKind::Class(ref class) => {
            let class = render_class(class);

            quote! { #HirType::class(#class) }
        }
        HirKind::Look(look) => {
            let look = render_look(look);

            quote! { #HirType::look(#look) }
        }
        HirKind::Repetition(Repetition {
            min,
            max,
            greedy,
            ref sub,
        }) => {
            let max = match max {
                None => quote! { ::core::option::Option::None },
                Some(max) => quote! { ::core::option::Option::Some(#max) },
            };

            let sub = hir_expression(sub);

            quote! {
                #HirType::repetition(#HirMod::Repetition {
                    min: #min,
                    max: #max,
                    greedy: #greedy,
                    sub: ::std::boxed::Box::new(#sub),
                })
            }
        }
        HirKind::Capture(Capture {
            index,
            ref name,
            ref sub,
        }) => {
            let name = match name.as_deref() {
                None => quote! { ::core::option::Option::None },
                Some(name) => quote! {
                    ::core::option::Option::Some(
                        ::core::convert::From::from(#name)
                    )
                },
            };

            let sub = hir_expression(sub);

            quote! {
                #HirType::capture(#HirMod::Capture {
                    index: #index,
                    name: #name,
                    sub: ::std::boxed::Box::new(#sub),
                })
            }
        }
        HirKind::Concat(ref concat) => {
            let concat = concat.iter().map(hir_expression);

            quote! {
                #HirType::concat(::std::vec::Vec::from([#(#concat,)*]))
            }
        }
        HirKind::Alternation(ref alternation) => {
            let alternation = alternation.iter().map(hir_expression);

            quote! {
                #HirType::alternation(::std::vec::Vec::from([#(#alternation,)*]))
            }
        }
    }
}
