// use proc_macro2::TokenStream as TokenStream2;
// use quote::ToTokens;
// use quote::quote_each_token

// macro_rules! make_lazy_quote {
//     (|$tokens:ident| $block:expr) => {{
//         struct Tokens<F: Fn(&TokenStream2)>(F);

//         impl<F: Fn(&mut TokenStream2)> ToTokens for Tokens<F> {
//             fn to_tokens(&self, tokens: &mut TokenStream2) {
//                 (self.0)(tokens)
//             }
//         }

//         Tokens(move |$tokens| $block)
//     }};
// }

// macro_rules! lazy_quote {
//     ({$($t:tt)*}) => {
//         make_lazy_quote!{ |tokens| quote_each_token!(tokens $($t)*) }
//     }

//     (match $switch:expr {
//         $(
//             $pattern:pat => {$($t:tt)}
//         )
//     })
// }
