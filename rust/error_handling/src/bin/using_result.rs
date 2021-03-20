use std::fs::File;
use std::io::{ErrorKind, Read, Error};

fn read_some_file() -> Result<String, std::io::Error> {
    let mut f = match File::open("/home/carl/invalid.txt") {
        Ok(file) => file,
        Err(err) => return Err(err),
    };

    let mut txt = String::new();
    if let Ok(bytes) = f.read_to_string(&mut txt) {
        println!("Read {} bytes successfully!", bytes);
    } else {
        return Err(std::io::Error::new(
            ErrorKind::InvalidData,
            "It did not work!",
        ));
    }

    Ok(txt)
}

fn main() -> Result<(), std::io::Error> {
    let txt = read_some_file();

    match txt {
        Ok(x) => {}
        Err(error) => {
            match error.kind() {
                ErrorKind::NotFound => {
                    println!("The file does not exist, try creating it or check for typos.");
                }
                ErrorKind::PermissionDenied => {
                    println!("You don't have access to this!")
                }
                _ => {}
            }
        }
    };

    Ok(())

    // if let Ok(data) = txt {
    //     println!("The file contains:\n{}", data);
    //     Ok(())
    // } else {
    //     Err(txt.err().unwrap())
    // }
}
