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
        let diffuse = (matte_color * matte_coefficient) * PI_DIVIDED;
        Self {
            ambient_color, ambient_coefficient, diffuse,
        }
    }
}

impl Material for Matte {
    fn bounce(&self, hit_point: &HitPoint, light: &PointLight, _ray: &Ray) -> Color {
        let ld = light.get_direction_from_point(&hit_point);
        let n = hit_point.normal.clone();

        let dp = n * ld;
        let lc = light.get_color();

        // determine the colour
        if dp > 0.0 {
            let roundness = lc * dp;
            roundness * self.diffuse
        } else {
            color::BLACK
        }
    }

    fn ambient_color_with_light(&self, ambient_light: &AmbientLight) -> Color {
        self.ambient_color * self.ambient_coefficient * ambient_light.get_color()
    }
}