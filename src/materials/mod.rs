use crate::{core::{hitpoint::HitPoint, color::Color, ray::Ray}, lights::{point_light::PointLight, ambient_light::AmbientLight}};

pub mod matte;
pub mod phong;

const PI_DIVIDED: f32 = 1. / std::f32::consts::PI;

pub trait Material {
    fn bounce(&self, hit_point: &HitPoint, light: &PointLight, ray: &Ray) -> Color;

    fn ambient_color_with_light(&self, ambient_light: &AmbientLight) -> Color;
}