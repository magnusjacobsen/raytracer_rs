use crate::core::{color::Color, point::Point, vector::Vector, ray::Ray, hit_point::HitPoint};

use super::Light;

#[derive(Debug)]
pub struct PointLight {
    color: Color,
    intensity: f64,
    pub position: Point,
}

impl PointLight {
    pub fn new(color: Color, intensity: f64, position: Point) -> Self {
        Self {color, intensity, position}
    }

    pub fn get_direction_from_point(&self, hit_point: &HitPoint) -> Vector {
        (self.position - hit_point.point).normalize()
    }

    pub fn get_shadow_ray(&self, hit_point: &HitPoint) -> Ray {
        let p = hit_point.escaped_point.clone();
        let direction = (self.position - p).normalize();

        Ray::new(p, direction)
    }
}

impl Light for PointLight {
    fn get_color(&self) -> Color {
        self.color * self.intensity
    }
}