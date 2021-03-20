use std::fs::File;
use std::io::{Read, Error};
use anyhow::{Result, Context};
use rand::Rng;

const FILE_TO_READ: &str = "Cargasdo.toml";

fn read_the_file() -> Result<String> {
    let mut f = File::open(FILE_TO_READ).context(format!("Opening file {}", FILE_TO_READ))?;
    let mut buf = String::new();
    f.read_to_string(&mut buf)?;

    Ok(buf)
}


fn main() -> Result<()> {

    let txt = read_the_file()?;
    println!("TEXT IS:\n{}", txt);

    Ok(())

}