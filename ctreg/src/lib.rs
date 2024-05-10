/*!
`ctreg` (pronounced cuh-tredge, in the style of Cthulhu) is a macro that uses
compile-time processing to make your regular expressions faster and easier to
use. See the [`regex`] macro for details.

```
use ctreg::regex;

// Create a regular expression with the macro. This regular expression is
// evaluated at compile time and its parsed, normalized representation is
// emitted as the `HelloWorld` type.
regex! { pub HelloWorld = "(?<greeting>[a-zA-Z0-9-_.]+)(, (?<target>[a-zA-Z0-9-_.]+))?!" }

// Create an instance of the regular expression.
let regex = HelloWorld::new();

// Use `is_match` to test if there was a match
assert!(regex.is_match("Hello, World!"));
assert!(regex.is_match("Goodbye!"));
assert!(!regex.is_match("Nothing to see here."));

// Use `find` to find the location of a match
let cap = regex.find("abc Greetings, Rustacean! 123").unwrap();
assert_eq!(cap.content, "Greetings, Rustacean!");
assert_eq!(cap.start, 4);
assert_eq!(cap.end, 25);

assert!(regex.find("Nothing to see here.").is_none());

// Use `captures` to find all of the named capture groups of a match (`greeting`
// and `target`, in this case). Capture groups are emitted at compile time and
// evaluated infallibly.
let groups = regex.captures("ah, Bonjour, reader!").unwrap();
assert_eq!(groups.greeting.content, "Bonjour");
assert_eq!(groups.target.unwrap().content, "reader");

let groups = regex.captures("This is goodbye!").unwrap();
assert_eq!(groups.greeting.content, "goodbye");
assert!(groups.target.is_none());

assert!(regex.captures("nothing to see here.").is_none());
```
*/

#[doc(hidden)]
pub mod ඞ {
    pub use ::regex_automata;
    pub use ::regex_syntax;
}

#[doc(hidden)]
pub use ctreg_macro::regex_impl;

/**
Create a type representing a regular expression. See the [module docs][crate]
for an example.

This macro creates a type, called `$Type`, representing the given `$regex`. The
regular expression is evaluated at program compile time, and the `$Type` is
emitted containing its pre-parsed, pre-normalized representation, with a
regex-like API for searching and matching capture groups. See the
[`demo::HelloWorld`] type for an example of the methods it generates.

Additionally, it creates a type called `${Type}Captures`, which contains a
[`Capture`] field for each named capture group in the regular expression. See
the [`demo::HelloWorldCaptures`] type for an example of this. The
[`captures`][demo::HelloWorld::captures] method performs a capturing search,
which returns this type. This search is evaluated infallibly: all groups that
are unconditionally present in the regular expression are also present in the
captures type. Any groups that are optional or part of an alternation appear as
an `Option<Capture>`. Named capture groups cannot be part of repetitions, since
there isn't a sensible thing to capture.

To keep the API simple and usable, anonymous capture groups are not present in
the capture groups, and are treated identically to non-capturing groups.

If the regex has no named capture groups, no `captures` method or `Captures`
type is generated.

Because it is not yet possible to create a regular expression in a `const`
context, this macro operates by creating a type instead of an object; this
types constructor builds a regex at runtime using the post-parse [normalized
form](https://docs.rs/regex-syntax/latest/regex_syntax/hir/struct.Hir.html) of
the expression. This means that some (but not all) of the runtime costs of
constructing the internal `Regex` object are ameliorated. In the spirit of
0-cost abstraction, we currently ask the caller to use their own `OnceLock` or
whatever other abstraction is appropriate to manage the creation and lifespan
of this object. This may change in the future.
*/
#[macro_export]
macro_rules! regex {
    ($Type:ident = $regex:literal) => {
        $crate::regex_impl! { $Type = $regex }
    };

    (pub $Type:ident = $regex:literal) => {
        $crate::regex_impl! { pub $Type = $regex }
    };
}

/**
Represents a single match of a regex in a haystack. It contains `start` and
`end`, which are byte offsets of the location of the match, as well as the
actual `content` of the match.

This type is used by [`find`](demo::HelloWorld::find) to indicate the overall
location of the match, and by [`captures`](demo::HelloWorld::captures), which
returns a separate [`Capture`] for each named capture group that matched.

This type is equivalent to the [`Match`](https://docs.rs/regex/latest/regex/struct.Match.html)
type from the `regex` crate
 */
#[derive(Debug, Clone, Copy)]
pub struct Capture<'a> {
    pub start: usize,
    pub end: usize,
    pub content: &'a str,
}

/**
Demo module, showing the types created by the [`regex`] macro.

This module contains the output of:

```
# use ctreg::regex;
regex! { pub HelloWorld = "(?<greeting>[a-zA-Z0-9-_.]+)(, (?<target>[a-zA-Z0-9-_.]+))?!" }
```
*/
pub mod demo {
    use super::Capture;

    /**
    Example regular expression object.

    This type is the output of the [`regex`] macro, compiling the regular expression:

    ```text
    (?<greeting>[a-zA-Z0-9-_.]+)(, (?<target>[a-zA-Z0-9-_.]+))?!
    ```

    It matches strings like `"Hello, World!`" and `"Goodbye!"`. It is provided
    here as an example of the type and methods created by [`regex`].

    See also the [`HelloWorldCaptures`] type, which is the generated type for
    getting capture groups.
    */
    #[derive(Debug, Clone)]
    pub struct HelloWorld {
        regex: ::regex_automata::meta::Regex,
    }

    impl HelloWorld {
        /**
        Construct a new instance of this regular expression object
        */
        #[inline]
        #[must_use]
        pub fn new() -> Self {
            let hir: ::regex_syntax::hir::Hir = ::regex_syntax::hir::Hir::concat(Vec::from([
                ::regex_syntax::hir::Hir::capture(::regex_syntax::hir::Capture {
                    index: 1u32,
                    name: Some(From::from("greeting")),
                    sub: Box::new(::regex_syntax::hir::Hir::repetition(
                        ::regex_syntax::hir::Repetition {
                            min: 1u32,
                            max: None,
                            greedy: true,
                            sub: Box::new(::regex_syntax::hir::Hir::class(
                                ::regex_syntax::hir::Class::Unicode(
                                    ::regex_syntax::hir::ClassUnicode::new([
                                        ::regex_syntax::hir::ClassUnicodeRange::new('-', '.'),
                                        ::regex_syntax::hir::ClassUnicodeRange::new('0', '9'),
                                        ::regex_syntax::hir::ClassUnicodeRange::new('A', 'Z'),
                                        ::regex_syntax::hir::ClassUnicodeRange::new('_', '_'),
                                        ::regex_syntax::hir::ClassUnicodeRange::new('a', 'z'),
                                    ]),
                                ),
                            )),
                        },
                    )),
                }),
                ::regex_syntax::hir::Hir::repetition(::regex_syntax::hir::Repetition {
                    min: 0u32,
                    max: Some(1u32),
                    greedy: true,
                    sub: Box::new(::regex_syntax::hir::Hir::concat(Vec::from([
                        ::regex_syntax::hir::Hir::literal(*b", "),
                        ::regex_syntax::hir::Hir::capture(::regex_syntax::hir::Capture {
                            index: 2u32,
                            name: Some(From::from("target")),
                            sub: Box::new(::regex_syntax::hir::Hir::repetition(
                                ::regex_syntax::hir::Repetition {
                                    min: 1u32,
                                    max: None,
                                    greedy: true,
                                    sub: Box::new(::regex_syntax::hir::Hir::class(
                                        ::regex_syntax::hir::Class::Unicode(
                                            ::regex_syntax::hir::ClassUnicode::new([
                                                ::regex_syntax::hir::ClassUnicodeRange::new(
                                                    '-', '.',
                                                ),
                                                ::regex_syntax::hir::ClassUnicodeRange::new(
                                                    '0', '9',
                                                ),
                                                ::regex_syntax::hir::ClassUnicodeRange::new(
                                                    'A', 'Z',
                                                ),
                                                ::regex_syntax::hir::ClassUnicodeRange::new(
                                                    '_', '_',
                                                ),
                                                ::regex_syntax::hir::ClassUnicodeRange::new(
                                                    'a', 'z',
                                                ),
                                            ]),
                                        ),
                                    )),
                                },
                            )),
                        }),
                    ]))),
                }),
                ::regex_syntax::hir::Hir::literal(*b"!"),
            ]));
            let regex = ::regex_automata::meta::Regex::builder()
                .build_from_hir(&hir)
                .expect("regex compilation failed, despite compile-time verification");
            Self { regex }
        }

        /**
        Test if this regular expression matches the `haystack` string, without
        getting any information about the location of the match.

        Prefer this method if you only care *that* there was a match, as it might
        be faster than [`find`][HelloWorld::find] or
        [`captures`][HelloWorld::captures].
        */
        #[inline]
        #[must_use]
        pub fn is_match(&self, haystack: &str) -> bool {
            self.regex.is_match(haystack)
        }

        /**
        Find the first match of this regex in the `haystack`, and return it as a
        [`Capture`].

        Prefer this method if you only care about the overall location of a match
        in the haystack, without regard for the specific capture groups.
        */
        #[inline]
        #[must_use]
        pub fn find<'i>(&self, haystack: &'i str) -> Option<Capture<'i>> {
            let capture = self.regex.find(haystack)?;
            let span = capture.span();
            let start = span.start;
            let end = span.end;
            let content = unsafe { haystack.get_unchecked(start..end) };
            Some(Capture {
                start,
                end,
                content,
            })
        }
    }

    impl Default for HelloWorld {
        fn default() -> Self {
            Self::new()
        }
    }

    impl HelloWorld {
        /**
        Search for the first match of this regex in the `haystack`, and return
        an object containing all of the named capture groups that were found.
        */
        #[inline]
        #[must_use]
        pub fn captures<'i>(&self, haystack: &'i str) -> Option<HelloWorldCaptures<'i>> {
            let mut slots = [None; (2usize + 1) * 2];
            let _ = self
                .regex
                .search_slots(&::regex_automata::Input::new(haystack), &mut slots)?;

            Some(HelloWorldCaptures {
                #[allow(clippy::blocks_in_conditions)]
                greeting: match {
                    let slot_start = slots[2usize];
                    let slot_end = slots[3usize];
                    match slot_start {
                        None => None,
                        Some(start) => {
                            let start = start.get();
                            let end = unsafe { slot_end.unwrap_unchecked() }.get();
                            let content = unsafe { haystack.get_unchecked(start..end) };
                            Some(Capture {
                                start,
                                end,
                                content,
                            })
                        }
                    }
                } {
                    Some(capture) => capture,
                    None => unsafe { core::hint::unreachable_unchecked() },
                },
                target: {
                    let slot_start = slots[4usize];
                    let slot_end = slots[5usize];
                    match slot_start {
                        None => None,
                        Some(start) => {
                            let start = start.get();
                            let end = unsafe { slot_end.unwrap_unchecked() }.get();
                            let content = unsafe { haystack.get_unchecked(start..end) };
                            Some(Capture {
                                start,
                                end,
                                content,
                            })
                        }
                    }
                },
            })
        }
    }

    /**
    Example captures object.

    This type is the output of the [`regex`] macro for the capture groups
    returned by the [`HelloWorld`] expression.
     */
    #[derive(Debug, Clone, Copy)]
    pub struct HelloWorldCaptures<'a> {
        /**
        The greeting is an non-optional [`Capture`], because there will always
        be a greeting when the expression matches.
         */
        pub greeting: Capture<'a>,

        /**
        The target is an optional [`Capture`], because the group is inside an
        `()?` optional group, so it may not be present even if the expression
        matched. Optional groups are also created by alternations.
        */
        pub target: Option<Capture<'a>>,
    }
}
