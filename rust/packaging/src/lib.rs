pub mod fare;
pub mod taxi;

/// Speed level for taxi trip
pub enum SpeedLevel {
    Slow,
    Limit,
    Fine,
    Jail
}

pub fn compute_fare(rate: f32, dist: f32, speed_level: SpeedLevel) -> f32 {
    (rate * dist + 25.0) * detail::speed_to_price(speed_level)
}

fn print_log(msg: &str) {
    println!("LOG: {}", msg);
}

mod detail {
    use crate::{SpeedLevel, print_log};

    pub fn speed_to_price(speed_level: SpeedLevel) -> f32 {
        print_log("Get fucked and die.");

        match speed_level {
            SpeedLevel::Slow => 0.7,
            SpeedLevel::Limit => 1.0,
            SpeedLevel::Fine => 1.5,
            SpeedLevel::Jail => 4.0,
        }
    }
}