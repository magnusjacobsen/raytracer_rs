use crate::core::{color::Color, point::Point, vector::Vector, ray::Ray};

use super::Light;


pub struct PointLight {
    color: Color,
    intensity: f32,
    pub position: Point,
}

impl PointLight {
    pub fn new(color: Color, intensity: f32, position: Point) -> Self {
        Self {color, intensity, position}
    }

    pub fn get_direction_from_point(&self, p: &Point) -> Vector {
        self.position.subtract(p).normalize()
    }

    pub fn get_shadow_ray(&self, p: &Point) -> Ray {
        let direction = self.get_direction_from_point(p);
        
        Ray::new(p.clone(), direction)
    }
}

impl Light for PointLight {
    fn get_color(&self) -> Color {
        self.color.scale(self.intensity)
    }
}