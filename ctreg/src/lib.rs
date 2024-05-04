#[doc(hidden)]
pub mod private {
    pub use ::regex_automata;
    pub use ::regex_syntax;
}

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

macro_rules! regex {
    (static $NAME:ident: $Type:ident = $regex:literal) => {};
}

pub struct Group<'a> {
    pub begin: usize,
    pub end: usize,
    pub content: &'a str,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
