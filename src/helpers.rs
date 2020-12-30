use std::f32::consts::PI;
use std::cmp::min;
use ggez::input::gamepad::gilrs::Button;
use std::mem::transmute;

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