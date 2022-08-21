use crate::{core::{color::{Color, self}, hitpoint::HitPoint, ray::Ray}, lights::{ambient_light::AmbientLight, point_light::PointLight, Light}};

use super::{PI_DIVIDED, Material};

pub struct Phong {
    ambient_color: Color,
    ambient_coefficient: f32,
    matte_color: Color,
    matte_coefficient: f32,
    specular_color: Color,
    specular_coefficient: f32,
    specular_exponent: i32,
    diffuse: Color,
}

impl Phong {
    pub fn new(ambient_color: Color,
        ambient_coefficient: f32,
        matte_color: Color,
        matte_coefficient: f32,
        specular_color: Color,
        specular_coefficient: f32,
        specular_exponent: i32) -> Self {
        
        let diffuse = matte_color.scale(matte_coefficient).scale(PI_DIVIDED);

        Self {
            ambient_color, ambient_coefficient, matte_color, matte_coefficient, specular_color, specular_coefficient, specular_exponent, diffuse,
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

impl Material for Phong {
    fn bounce(&self, hit_point: &HitPoint, light: &PointLight, ray: &Ray) -> Color {
        let ld = light.get_direction_from_point(&hit_point.point);
        let rd = ray.direction.normalize();
        let n = hit_point.normal.clone();

        let dot_product = hit_point.normal.dot_product(&ld);

        let r1 = ld.negate().add(&n.multiply_scalar(2.0 * n.dot_product(&ld)));

        // determine the colour
        if dot_product > 0.0 {
            let specular = if rd.negate().dot_product(&r1) > 0.0 {
                self.specular_color
                    .scale(
                        self.specular_coefficient
                    )
                    .scale(
                        rd.negate().dot_product(&r1).powi(self.specular_exponent)
                    )
            } else {
                color::BLACK
            };

            let direction = light.get_color().scale(n.dot_product(&ld));

            self.diffuse.add(specular).multiply(direction)
        } else {
            color::BLACK
        }
    }
}

/*
        // Detemine the colour ()
        if n * ld > 0. then

            // The standard diffuse colour
            let matte = (matteCoefficient * matteColour) * pidivided
            
            // The specular colour
            let specular = 
                if r1 * -rd > 0. then
                    ks * cs * ((r1 * (-rd)) ** float(e))
                else
                    Colour.Black
            let direction = lc * (n * ld)
            
            // The final colour
            (matte + specular) * direction
        else
            Colour.Black

            */