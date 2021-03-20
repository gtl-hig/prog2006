/// Compute fare for taxi ride
pub fn compute_fare(rate: f32, distance: f32) -> f32 {
    rate * distance + 100.0
}
