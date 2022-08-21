use crate::core::color::Color;

use super::Light;

pub struct AmbientLight {
    color: Color,
    intensity: f32,
}

impl AmbientLight {
    pub fn new(color: Color, intensity: f32) -> Self {
        Self {color, intensity}
    }
}

impl Light for AmbientLight {
    fn get_color(&self) -> Color {
        self.color.scale(self.intensity)
    }
}