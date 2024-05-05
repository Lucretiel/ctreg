use ctreg::regex;

regex! { HelloWorld = r"(?<greeting>(Hello|Goodbye))(, (?<target>[a-zA-Z0-9]+))?!" }

fn main() {
    let regex = HelloWorld::new();

    let haystack = "Hello, World! abcdef";

    assert!(regex.is_match(haystack));

    let cap = regex.find(haystack).unwrap();
    assert_eq!(cap.content, "Hello, World!");

    let matches = regex.captures(haystack).unwrap();
    assert_eq!(matches.greeting.content, "Hello");
    assert_eq!(matches.target.unwrap().content, "World");

    let haystack = "Goodbye!";
    let matches = regex.captures(haystack).unwrap();
    assert_eq!(matches.greeting.content, "Goodbye");
    assert!(matches.target.is_none());
}
