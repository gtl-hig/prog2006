
// Collections

// String, &str and functions
//   String::from("literal")
//   macro format!
//   "literal".to_string() and .to_owned()
//
// boundary of bytes vs boundary of characters, and UTF8, encoding
// "Cześć!"
//
// simple parsing with split_
// temporary variables
//
// iterating with
//   bytes()
//   chars()
//   UTF boundary
// [] and get

fn string_examples() {
    let hello = "Hello".to_owned();
    let hello_pl = "Cześć".to_owned();
    // Note, one cannot do hello[0] - no indexing of string characters like that
    // consider bytes vs characters semantics. Some characters are longer than single byte.
    println!("{} length is {}", &hello, hello.len());
    println!("{} length is {}", &hello_pl, hello_pl.len());

    println!("first character of {} is {}", &hello, hello.chars().nth(0).unwrap());
    println!("first character of {} is {}", &hello_pl, hello_pl.chars().nth(0).unwrap());
    // note, if we attempt unwrap on None, we get panic!
    // We have to get characters on the proper character boundary.
}



// Vec, vec! macro, simple iterators
// Same type.  Note about enums.
//   push()
//   [] and get

// Turns out, one cannot derive Display trait! One needs to write it all by hand!
// Just saying, in Haskell, they figured out how to do default
// implementation so it is possible ;)
#[derive(Debug)]
enum Value {
    Num (i32),
    Op (String),
}

// simple vector example
fn vector_example() {
    // use constructor
    let mut list = Vec::new();
    list.push(1);
    list.push(2);

    // or a macro
    let list2 = vec!(1, 2);
    // note, with the macro our list2 can be immutable

    // show values with [] will panic if we go out of bounds
    println!("Item list2[0] = {}", list2[0]); // that will work, but, if you
    //put index above 1,/it will crash
    println!("Item list2[2] = {:?} // out of bounds", list2.get(2)); // this will work because we
    //get Option back

    let _result2 = list2.get(2);

    // we can keep in a vector things of the same type, only.
    // enums are useful for wrapping complex data in the same vector
    // for example for 2, 4, *
    let prog = vec!(Value::Num(2), Value::Num(4), Value::Op("*".to_owned()));

    for item in prog {
        println!("{:?}", item);
    }
}


// HashMap
//   insert(key, value)
//   entry(key), entry().or_insert()



fn main() {
    println!("\n\nString Examples\n");
    string_examples();

    println!("\n\nVector Examples\n");
    vector_example();

    println!("That's all folks");
}



