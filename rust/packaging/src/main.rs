use packaging::{compute_fare, taxi, fare};

fn main() {
    println!("Jail Taxi costs: NOK {}", compute_fare(32.5, 5.4, packaging::SpeedLevel::Jail));
    println!("The taxi costs: NOK {}", taxi::compute_fare(12.5, 34.5));
    println!("The other taxi costs: NOK {}", fare::compute_fare(34.5, 12.5));

    fare::trips::save_trip(32.0, "Jenny");
}
