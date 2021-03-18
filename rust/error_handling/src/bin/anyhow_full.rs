use anyhow::{anyhow, Context, Result};
use std::ops::RangeInclusive;
use thiserror::Error;
use rand::Rng;

#[derive(Error, Debug)]
enum IntRangeError {
    #[error("Number ({input:?}) out of range. Expected range is {range:?}")]
    OutOfRange {
        input: i32,
        range: RangeInclusive<i32>,
    },
    #[error("It just failed, that's it")]
    ItJustFailed
}

/// Read an integer in the given inclusive range
fn read_int_in_range(range: RangeInclusive<i32>) -> Result<i32> {
    println!("Enter a number in range [{:?}]: ", range);

    let mut buf = String::new();
    std::io::stdin()
        .read_line(&mut buf)
        .context("Reading input")?;

    let number: i32 = buf
        .trim()
        .parse()
        .context("Parsing the user input number")?;

    let mut rng = rand::thread_rng();
    if rng.gen_range(0..100) > 50 {
        return Err(anyhow!(IntRangeError::ItJustFailed));
    }

    if range.contains(&number) {
        Ok(number)
    } else {
        Err(anyhow!(IntRangeError::OutOfRange {
            input: number,
            range: range
        }))
    }
}

fn main() -> Result<()> {

    let birth_year = read_int_in_range(1900..=2021);

    match birth_year {
        Ok(year) => { println!("Birth year: {}", year); }
        Err(error) => {
            match error.downcast_ref::<IntRangeError>().unwrap() {
                IntRangeError::OutOfRange { input, range } => {
                    eprintln!("The out of range is caused by your input being invalid.");
                    eprintln!("{:?}", error);
                }
                IntRangeError::ItJustFailed => {
                    eprintln!("Well, that is not fun!");
                }
            }
        }
    }

    Ok(())
}
