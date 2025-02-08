# ctreg

<!-- cargo-rdme start -->

`ctreg` (pronounced cuh-tredge, in the style of Cthulhu) is a macro providing
static typing to your regular expressions, allowing syntax errors to be detected
at compile time and capture groups to be matched infallibly.

```rust
use ctreg::regex;

// Create a regular expression with the macro. This regular expression is
// analyzed at compile time and its parsed, normalized representation is
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

<!-- cargo-rdme end -->
