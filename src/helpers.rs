use std::f32::consts::PI;
use ggez::input::gamepad::gilrs::Button;
use std::mem::transmute;
use ggez::graphics::Color;
use ggez::nalgebra::{Vector2, Point2};

pub fn angle_diff_abs(a1: f32, a2: f32) -> f32 {
    let diff1 = (a1 - a2).abs();
    let diff2 = 2.0 * PI - diff1;

    if diff1 < diff2 { diff1 } else { diff2 }
}

pub struct Timer {
    duration: f32
}

impl Timer {
    pub fn new() -> Timer {
        Timer { duration: 0.0 }
    }

    pub fn check(&mut self, dt: f32, duration_condition: f32) -> bool {
        self.duration += dt;
        if self.duration >= duration_condition {
            self.duration -= duration_condition;
            true
        } else {
            false
        }
    }

    pub fn reset(&mut self) {
        self.duration = 0.0;
    }
}

pub fn lerp_colors(col1: &Color, col2: &Color, col1_factor: f32) -> Color {
    let col2_factor = 1.0 - col1_factor;
    Color::new(
        col1.r * col1_factor + col2.r * col2_factor,
        col1.g * col1_factor + col2.g * col2_factor,
        col1.b * col1_factor + col2.b * col2_factor,
        1.0
    )
}

pub fn btn_to_u16(btn: Button) -> u16 {
    unsafe { transmute::<Button, u16>(btn) }
}

pub fn u16_to_btn(u: u16) -> Option<Button> {
    if u < 20 {
        Some(unsafe { transmute::<u16, Button>(u) })
    } else {
        None
    }
}

// To find orientation of ordered triplet (p, q, r).
// The function returns following values
// 0 --> p, q and r are colinear
// 1 --> Clockwise
// 2 --> Counterclockwise
fn orientation(p: Point2<f32>, q: Point2<f32>, r: Point2<f32>) -> i32
{
    // See https://www.geeksforgeeks.org/orientation-3-ordered-points/
    // for details of below formula.
    let val = (q.y - p.y) * (r.x - q.x) -
                (q.x - p.x) * (r.y - q.y);

    if val == 0. { return 0; } // colinear

    return if val > 0. { 1 } else { 2 }; // clock or counterclock wise
}

/// Intersection here means that they actually cross
/// (i.e. one goes THROUGH the other, one point lying on the other is not enough)
pub fn intersect(start1: Point2<f32>, end1: Point2<f32>, start2: Point2<f32>, end2: Point2<f32>) -> bool {
    // Find the four orientations needed for general and
    // special cases
    let o1 = orientation(start1, end1, start2);
    let o2 = orientation(start1, end1, end2);
    let o3 = orientation(start2, end2, start1);
    let o4 = orientation(start2, end2, end1);

    // General case
    if o1 != o2 && o3 != o4 {
        return true;
    }

    // Special Cases (not of interest to me)
    /*
    // p1, q1 and p2 are colinear and p2 lies on segment p1q1
    if (o1 == 0 && onSegment(p1, p2, q1)) return true;

    // p1, q1 and q2 are colinear and q2 lies on segment p1q1
    if (o2 == 0 && onSegment(p1, q2, q1)) return true;

    // p2, q2 and p1 are colinear and p1 lies on segment p2q2
    if (o3 == 0 && onSegment(p2, p1, q2)) return true;

    // p2, q2 and q1 are colinear and q1 lies on segment p2q2
    if (o4 == 0 && onSegment(p2, q1, q2)) return true;
    */

    false
}