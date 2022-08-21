use crate::core::color::Color;

pub mod point_light;
pub mod ambient_light;

pub trait Light {
    fn get_color(&self) -> Color;
}