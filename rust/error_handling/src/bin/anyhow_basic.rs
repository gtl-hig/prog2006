use anyhow::{Context, Result};
use std::fs::File;
use std::io::Read;

const FILE_TO_OPEN: &str = "Cargo.toml";

fn main() -> Result<()> {
    let mut f = File::open(FILE_TO_OPEN).context(format!("Opening {}", FILE_TO_OPEN))?;

    let mut txt = String::new();
    f.read_to_string(&mut txt)
        .context(format!("Reading {}", FILE_TO_OPEN))?;

    println!("The file contains:\n{}", txt);

    Ok(())
}
