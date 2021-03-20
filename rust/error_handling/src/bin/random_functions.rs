use anyhow::{anyhow, Context, Result};
use rand::Rng;

fn panic_fail() -> i32 {
    let mut rng = rand::thread_rng();
    let x = rng.gen_range(0..10);

    if x > 5 {
        x
    } else {
        panic!("Number out of range!")
    }
}


fn result_fail() -> Result<i32> {
    let mut rng = rand::thread_rng();
    let x = rng.gen_range(0..10);

    if x > 5 {
        Ok(x)
    } else {
        Err(anyhow!("Number out of range!").context("Invalid input."))
    }
}

fn repeat_until_success() {
    loop {
        match result_fail() {
            Ok(n) => {
                println!("We got {}", n);
                break;
            }
            Err(err) => eprintln!("Retrying due to Error: {:?}", err),
        }
    }
}

fn main() {
    let num = panic_fail();
    println!("We first got {}", num);
    repeat_until_success();
}
