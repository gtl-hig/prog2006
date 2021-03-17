use std::fs::File;
use std::io::Read;

fn main() -> Result<(), std::io::Error> {
    let mut f = File::open("Cargo.toml")?;

    let mut txt = String::new();
    f.read_to_string(&mut txt)?;

    println!("The file contains:\n{}", txt);

    Ok(())
}
