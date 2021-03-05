extern crate clap;
extern crate std;

use clap::{App, Arg};
use std::fs::File;
use std::io::Write;

fn main() {
    let matches = App::new("Students Data Generator")
        .version("1.0")
        .author("Mariusz fom PROG2006")
        .about("The program generates data file for testing student error handling implementations")
        .arg(Arg::with_name("correct")
            .short("c")
            .long("correct")
            .value_name("CORRECT")
            .help("How many correct students to generate")
            .takes_value(true)
            .required(true))
        .arg(Arg::with_name("incorrect")
            .short("i")
            .long("incorrect")
            .value_name("INCORRECT")
            .help("How many incorrect students to generate")
            .takes_value(true)
            .required(true))
        .arg(Arg::with_name("output")
            .short("o")
            .long("output")
            .value_name("OUTPUT_FILE")
            .help("Output filename to generate the data INTO.")
            .takes_value(true)
            .default_value("output.txt")
            .required(true))
        .get_matches();

    let mut correct: i32 = matches.value_of("correct").unwrap().parse().unwrap();
    let mut incorrect: i32 = matches.value_of("incorrect").unwrap().parse().unwrap();
    let path = matches.value_of("output").unwrap();

    println!("Generating {} studens: {} correct - {} incorrect",
             correct + incorrect, correct, incorrect);

    let mut output = File::create(path).unwrap();

    loop {
        if correct == 0 { break }
        write!(output, "new Aaaa Aaaa 20\n").unwrap();
        correct -= 1;
    }

    loop {
        if incorrect == 0 { break }
        write!(output, "new 1 1 a\n").unwrap();
        incorrect -= 1;
    }

    write!(output, "list\nend\n").unwrap();
}
