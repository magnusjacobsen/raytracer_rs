use crate::{core::{color::{Color, self}, hitpoint::HitPoint, ray::Ray}, lights::{ambient_light::AmbientLight, Light, point_light::PointLight}};

use super::{Material, PI_DIVIDED};

// just a Matte material
pub struct Matte {
    ambient_color: Color,
    ambient_coefficient: f32,
    //matte_color: Color,
    //matte_coefficient: f32,
    diffuse: Color,
}

impl Matte {
    pub fn new(ambient_color: Color, ambient_coefficient: f32, matte_color: Color, matte_coefficient: f32) -> Self {
        let diffuse = matte_color.scale(matte_coefficient).scale(PI_DIVIDED);
        Self {
            ambient_color, ambient_coefficient, diffuse,
        }
    }

    pub fn ambient_color_with_light(&self, ambient_light: AmbientLight) -> Color {
        self.ambient_color
            .scale(self.ambient_coefficient)
            .multiply(ambient_light.get_color())
    }

    pub fn reflection_factor(&self) -> Color {
        color::WHITE
    }
}

impl Material for Matte {
    fn bounce(&self, hit_point: &HitPoint, light: &PointLight, ray: &Ray) -> Color {
        let dot_product = hit_point.normal.dot_product(&light.get_direction_from_point(&hit_point.point));

        // determine the colour
        if dot_product > 0.0 {
            light.get_color().scale(dot_product).multiply(self.diffuse)
        } else {
            color::BLACK
        }
    }
}