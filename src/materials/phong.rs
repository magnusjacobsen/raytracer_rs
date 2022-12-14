use crate::{core::{color::{Color, self}, hit_point::HitPoint, ray::Ray}, lights::{ambient_light::AmbientLight, point_light::PointLight, Light}};

use super::{PI_DIVIDED, Material};

pub struct Phong {
    ambient_color: Color,
    ambient_coefficient: f64,
    //matte_color: Color,
    //matte_coefficient: f64,
    specular_color: Color,
    specular_coefficient: f64,
    specular_exponent: i32,
    diffuse: Color,
}

impl Phong {
    pub fn new(ambient_color: Color,
        ambient_coefficient: f64,
        matte_color: Color,
        matte_coefficient: f64,
        specular_color: Color,
        specular_coefficient: f64,
        specular_exponent: i32) -> Self {
        
        let diffuse = (matte_color * matte_coefficient) * PI_DIVIDED;

        println!("diff: {:?}", diffuse);

        Self {
            ambient_color, ambient_coefficient, specular_color, specular_coefficient, specular_exponent, diffuse,
        }
    }

    pub fn reflection_factor(&self) -> Color {
        color::WHITE
    }
}

impl Material for Phong {
    fn bounce(&self, hit_point: &HitPoint, light: &PointLight, ray: &Ray) -> Color {
        let ld = light.get_direction_from_point(&hit_point);
        let n = hit_point.normal.clone();
        let dp = n * ld;
        let r1 = (-ld) + n * (2.0 * dp);
        let rd = ray.direction.normalize();
        let lc = light.get_color();

        // determine the colour
        if dp > 0.0 {
            let r1_neg_rd = r1 * (-rd);

            let specular = if r1_neg_rd > 0.0 {
                self.specular_color * self.specular_coefficient * r1_neg_rd.powi(self.specular_exponent)
            } else {
                color::BLACK
            };

            let direction = lc * dp;
            (self.diffuse * 2.0 + specular) * direction
        } else {
            color::BLACK
        }
    }

    fn ambient_color_with_light(&self, ambient_light: &AmbientLight) -> Color {
        self.ambient_color * self.ambient_coefficient * ambient_light.get_color()
    }
}