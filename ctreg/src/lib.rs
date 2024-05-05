#[doc(hidden)]
pub mod ඞ {
    pub use ::regex_automata;
    pub use ::regex_syntax;
}

#[doc(hidden)]
pub use ctreg_macro::regex_impl;

#[macro_export]
macro_rules! regex {
    (pub $Type:ident = $regex:literal) => {
        $crate::regex_impl! { pub $Type = $regex }
    };

    ($Type:ident = $regex:literal) => {
        $crate::regex_impl! { $Type = $regex }
    };
}

#[derive(Debug, Clone, Copy)]
pub struct Capture<'a> {
    pub start: usize,
    pub end: usize,
    pub content: &'a str,
}
