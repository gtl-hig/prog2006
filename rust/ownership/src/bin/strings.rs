#[allow(unused_variables)]
fn main() {
    // Creating a &str
    let s = "Hello";

    // The above 's' is immutable and it is limited what we can do with it. Where would it be stored?

    // Creating Strings - These hold memory until they go out of scope
    let s = String::from("I become a string");
    let s = String::new();
    let s = "I also become a string".to_string();
    let s = "Maybe this is more clear".to_owned();
    let s = format!("With {} parameters, and {} of love", 2, "lots");

    // 's' is only valid within this scope
    {
        let mut s2 = "I like memory".to_owned();
        s2.push_str("... Not!");
        println!("Before: {}", s2);
    }

    let name = String::from("Mariusz");
    let name2 = name;

    // Will this work given the rules of ownership?
    // println!("Hello {} and {}", name, name2);

    let name = name2.clone();
    // println!("Hello {} and {}", name, name2);

    // Str does not care
    let name = "Bob";
    let name2 = name;
    println!("I am {} and {}", name, name2);

    // Function calls and ownership
    string_ref_parameters(&s);
    str_parameter("I like literals");
    str_parameter(&s);
    string_parameter(s);

    // Mutability and references / borrowing
    let mut s = "Good stuff".to_owned();

    let sref = &s;
    let sref2 = &s;

    // println!("Using {}, {}", sref, sref2);

    let mut s2 = "Bad stuff".to_owned();
    let s2ref = &s2;
    let s2ref2 = &s2;
    let s2ref3 = &mut s2;

    // println!("Using {}, {}", s2ref, s2ref2);

    // This can bite you a lot
    let mut s3 = "I will murder your brains".to_owned();
    let some_ref = &s3;
    s3.push_str(", Bob!");

    println!("Hello, {}", some_ref);
}

/// Takes ownership
fn string_parameter(s: String) {
    println!("I will destroy s: {}", s);
}

/// Borrows the whole String
fn string_ref_parameters(s: &String) {
    println!("I just look at s: {}", s);
}

/// Borrows a str
fn str_parameter(s: &str) {
    println!("I look at *any* string: {}", s);
}
