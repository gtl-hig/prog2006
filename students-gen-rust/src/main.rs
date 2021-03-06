extern crate clap;
extern crate std;

use anyhow::{Context, Result};
use clap::{App, Arg};
use std::fs::File;
use std::io::Write;
use rand::Rng;

// Some arrays of names to produce more combinations of good / bad names
const OK_FIRST_NAMES: [&str; 5] = ["Xi", "Petter", "Ola", "Vilde", "John"];
const OK_SURNAMES: [&str; 4] = ["Wang", "Pettersen", "Tronsmoen", "Baker"];
const BAD_FIRST_NAMES: [&str; 4] = ["x", "per", "o", "F"];
const BAD_SURNAMES: [&str; 3] = ["Wan", "pettersen", "T"];

fn main() -> Result<()> {
    let matches = App::new("Students Data Generator")
        .version("1.0")
        .author("Mariusz fom PROG2006")
        .about("The program generates data file for testing student error handling implementations")
        .arg(
            Arg::with_name("correct")
                .short("c")
                .long("correct")
                .value_name("CORRECT")
                .help("How many correct students to generate")
                .takes_value(true)
                .required(true),
        )
        .arg(
            Arg::with_name("incorrect")
                .short("i")
                .long("incorrect")
                .value_name("INCORRECT")
                .help("How many incorrect students to generate")
                .takes_value(true)
                .required(true),
        )
        .arg(
            Arg::with_name("output")
                .short("o")
                .long("output")
                .value_name("OUTPUT_FILE")
                .help("Output filename to generate the data INTO.")
                .takes_value(true)
                .default_value("output.txt")
                .required(true),
        )
        .get_matches();

    // Parse arguments with Clap, add error context to help the user if they mess up using anyhow
    let correct: i32 = matches
        .value_of("correct").unwrap()
        .parse()
        .with_context(|| "Correct should be a number")?;
    let incorrect: i32 = matches
        .value_of("incorrect").unwrap()
        .parse()
        .with_context(|| "Incorrect should be a number")?;
    let path = matches.value_of("output").unwrap();

    println!(
        "Generating {} students: {} correct - {} incorrect",
        correct + incorrect,
        correct,
        incorrect
    );


    let mut output = File::create(path)?;
    let mut rng = rand::thread_rng();

    // Generate the OK students
    for _ in 0..correct {
        let name: &str = OK_FIRST_NAMES[(rng.gen_range(0..OK_FIRST_NAMES.len()))];
        let surname: &str = OK_SURNAMES[(rng.gen_range(0..OK_SURNAMES.len()))];
        writeln!(output, "new {} {} {}", name, surname, rng.gen_range(18..=130))?;
    }

    // Generate the BAD students
    for _ in 0..incorrect {
        let name: &str = BAD_FIRST_NAMES[(rng.gen_range(0..BAD_FIRST_NAMES.len()))];
        let surname: &str = BAD_SURNAMES[(rng.gen_range(0..BAD_SURNAMES.len()))];
        writeln!(output, "new {} {} {}", name, surname, rng.gen_range(0..18))?;
    }

    write!(output, "list\nend\n")?;

    Ok(())
}
