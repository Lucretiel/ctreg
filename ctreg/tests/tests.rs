use cool_asserts::assert_matches;
use ctreg::{regex, Capture};

regex! {
    HelloWorld = r"(?<greeting>[a-zA-Z0-9-_.]+)(, (?<target>[a-zA-Z0-9-_.]+))?!"
}

#[test]
fn test_is_match() {
    let regex = HelloWorld::new();

    assert!(regex.is_match("Hello, World!"));
    assert!(!regex.is_match("nothing to see here"));
    assert!(regex.is_match("Panic! At the Repo"));
}

#[test]
fn test_find() {
    let regex = HelloWorld::new();

    let found = regex.find("abc Hello, World! Def").unwrap();
    assert_matches!(
        found,
        Capture {
            start: 4,
            end: 17,
            content: "Hello, World!"
        }
    );
}

#[test]
fn test_capture_miss() {
    let regex = HelloWorld::new();

    assert!(regex.captures("Nothing to see here.").is_none());
}

#[test]
fn test_captures() {
    let regex = HelloWorld::new();

    let caps = regex.captures("Hello, World!").unwrap();

    assert_matches!(
        caps,
        HelloWorldCaptures {
            greeting: Capture {
                start: 0,
                end: 5,
                content: "Hello"
            },
            target: Some(Capture {
                start: 7,
                end: 12,
                content: "World",
            })
        }
    )
}

#[test]
fn test_opt_capture() {
    let regex = HelloWorld::new();

    let caps = regex.captures("12344321!").unwrap();

    assert_matches!(
        caps,
        HelloWorldCaptures {
            greeting: Capture {
                start: 0,
                end: 8,
                content: "12344321"
            },
            target: None,
        }
    )
}
