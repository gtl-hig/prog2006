mod fare;
pub mod taxi;

#[derive(Copy, Clone)]
pub enum DrivingStyle {
    Slow,
    Limit,
    Fast,
    Jail,
}

pub struct FareParameters {
    dist: f32,
    rate: f32,
    style: DrivingStyle,
}

impl FareParameters {
    pub fn new(dist: f32, rate: f32, style: DrivingStyle) -> Self {
        FareParameters { dist, rate, style }
    }
}

pub fn compute_fare(params: &mut FareParameters) -> f32 {
    let base = fare::compute_fare(params.dist, params.rate);

    // Public in the module
    params.rate += 1.0;

    base * detail::map_style_to_price_multiplier(params.style)
}

fn log_message(msg: &str) {
    println!("LOG: {}", msg);
}

mod detail {
    use super::DrivingStyle;
    use crate::log_message;

    pub fn map_style_to_price_multiplier(style: DrivingStyle) -> f32 {
        super_detail::print_me();
        log_message("I steal from parent");

        match style {
            DrivingStyle::Slow => 0.5,
            DrivingStyle::Limit => 1.0,
            DrivingStyle::Fast => 1.33,
            DrivingStyle::Jail => 3.9,
        }
    }

    mod super_detail {
        pub fn print_me() {
            super::super::log_message("SUPER. DETAIL.");
        }
    }
}
