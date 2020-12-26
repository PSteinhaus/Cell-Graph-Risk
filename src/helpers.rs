use std::f32::consts::PI;
use std::cmp::min;

pub fn angle_diff_abs(a1: f32, a2: f32) -> f32 {
    let diff1 = (a1 - a2).abs();
    let diff2 = 2.0 * PI - diff1;

    if diff1 < diff2 { diff1 } else { diff2 }
}