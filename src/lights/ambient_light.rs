use crate::core::color::Color;

use super::Light;

pub struct AmbientLight {
    color: Color,
    intensity: f64,
}

impl AmbientLight {
    pub fn new(color: Color, intensity: f64) -> Self {
        Self {color, intensity}
    }
}

impl Light for AmbientLight {
    fn get_color(&self) -> Color {
        self.color * self.intensity
    }
}