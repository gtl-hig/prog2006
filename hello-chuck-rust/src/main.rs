use serde::Deserialize;

#[derive(Deserialize, Debug)]
struct Joke {
      categories: Vec<String>
    , icon_url:   String
    , id:         String
    , url:        String
    , value:      String
}

//
// Admittedly, enforcement of CAPS_SNAIL for constants makes sense ;)
//
const CHUCK_API: &str = "http://api.chucknorris.io/jokes/random";

//
// main entry point to the application
// note the explicit unwrapping Results to GET INTO
// the inner structure of the enum type, e.g. json() function cannot
// be called on Result
//
fn main() {
    let res = reqwest::blocking::get(CHUCK_API).expect("Error when doing GET");
    let joke: Joke = res.json().expect("Error when parsing JSON");
    println!("{}", joke.value)
}
