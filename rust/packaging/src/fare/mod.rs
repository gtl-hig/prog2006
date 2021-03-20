pub mod trips;

/// Compute fare for taxi ride
pub fn compute_fare(distance: f32, rate: f32) -> f32 {
    distance * rate + 50.0
}
