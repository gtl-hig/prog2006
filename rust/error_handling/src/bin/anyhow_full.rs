use anyhow::{anyhow, Context, Result};
use std::ops::RangeInclusive;
use thiserror::Error;

#[derive(Error, Debug)]
enum IntRangeError {
    #[error("Number ({input:?}) out of range. Expected range is {range:?}")]
    OutOfRange {
        input: i32,
        range: RangeInclusive<i32>,
    },
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
    let birth_year = read_int_in_range(1900..=2021)?;
    println!("Birth year: {}", birth_year);

    Ok(())
}
