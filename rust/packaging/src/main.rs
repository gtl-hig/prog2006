use taxi_ride::{taxi, DrivingStyle, FareParameters};

fn main() {
    println!("The Taxi costs: NOK {}", taxi::compute_fare(35.4, 11.2));
    // println!("The other Taxi costs: NOK {:.2}", fare::compute_fare(11.2, 30.4));

    let mut fare_parameters = FareParameters::new(12.0, 36.2, DrivingStyle::Slow);

    println!(
        "The slow Taxi costs: NOK {:.2}",
        taxi_ride::compute_fare(&mut fare_parameters)
    );
    println!(
        "The slow Taxi costs: NOK {:.2}",
        taxi_ride::compute_fare(&mut fare_parameters)
    );
    println!(
        "The slow Taxi costs: NOK {:.2}",
        taxi_ride::compute_fare(&mut fare_parameters)
    );
}
