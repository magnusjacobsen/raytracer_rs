//use rayon::prelude::*;
use lodepng;
use std::path::Path;

use crate::{scene::Scene, camera::pinhole::PinholeCamera, core::{color::{Color, self}, ray::Ray, hit_point::HitPoint}, lights::{point_light::PointLight}, shapes::base_shape::BaseShape};

pub struct Render {
    pub scene: Scene,
    pub camera: PinholeCamera,
}

impl Render {
    pub fn new(scene: Scene, camera: PinholeCamera) -> Self {
        Self {scene, camera}
    }

    pub fn cast(&self, ray: &Ray) -> Color {
        if let Some((hit, shape)) = self.get_closest_hit(ray) {
            let ambient_color = shape.material.ambient_color_with_light(&self.scene.ambient);
            
            // sum the light colors (and minus the shadow) for that hitpoint, and add them to the ambient color
            self.scene.lights.iter()
                .fold(ambient_color, |color, light| {
                    let light_color = shape.material.bounce(&hit, &light, &ray);
                    let shadow_color = self.cast_shadow(&hit, light);
                    color + (light_color - shadow_color)
                })
        } else {
            self.scene.background
        }
    }

    fn cast_shadow(&self, hit_point: &HitPoint, light: &PointLight) -> Color {
        let shadow_ray = light.get_shadow_ray(&hit_point);

        if let Some((new_hit, _)) = self.get_closest_hit(&shadow_ray) {
            if new_hit.time < light.position.distance(&hit_point.point).magnitude {
                return color::WHITE;
            }
        }
        color::BLACK
    }

    fn get_closest_hit(&self, ray: &Ray) -> Option<(HitPoint, &BaseShape)> {
        self.scene.shapes
            .iter()
            .fold(None, |prev, shape|
                match shape.hit_function.run(&ray) {
                    None        => prev,
                    Some(hit)   => 
                        if let Some((prev_hit, _)) = &prev {
                            if hit.time < prev_hit.time {
                                Some((hit, &shape))
                            } else {
                                prev
                            }
                        } else {
                            Some((hit, &shape))
                        }
                }
            )
    }

    fn create_image(&self) -> Vec<u8> {
        let coords = 
            (0..self.camera.res_y).map(|y|
                (0..self.camera.res_x).map(move |x|
                    (x, y)
                )
            )
            .flatten()
            .collect::<Vec<(usize, usize)>>();

        let mut buffer = vec![vec![color::WHITE; self.camera.res_x]; self.camera.res_y];

        let total_pixels = (self.camera.res_y * self.camera.res_x) as i32;
        let mut count = 0;
        let mut last= -1;

        /*coords
        .iter()
        .for_each(|(x, y)| {
            let rays = self.camera.create_rays(*x, *y);
            let color = rays
                .iter()
                .fold(color::BLACK, |color, ray| {
                    count += 1;
                    if (count * 100) / total_pixels > last {
                        last = (count * 100) / total_pixels;
                        println!("{}%", last);
                    }
                    color + self.cast(ray)
                }); rays.len() as f64;
            buffer[*y][*x] = color
        });*/

        for (x, y) in coords {
            count += 1;
            if (count * 100) / total_pixels > last {
                last = (count * 100) / total_pixels;
                println!("{}%", last);
            }
            let rays = self.camera.create_rays(x, y);
            let color = self.cast(&rays[0]);
            buffer[y][x] = color;
        }

        let sum = buffer.iter().fold(0, |acc, x| acc + x.iter().fold(0, |acc, color| acc + color.to_u8_vec().iter().map(|z| *z as i64).sum::<i64>()));
        println!("sum: {}", sum);

        buffer.into_iter()
            .map(|x| x.into_iter()
                .map(|color| 
                    color.to_u8_vec().into_iter()
                )
                .flatten()
            )
            .flatten()
            .collect()
    }

    pub fn save_image(&self, _path: &str) {
        let image = self.create_image();
        let path = &Path::new("test_file.png");

        lodepng::encode_file(path, &image, self.camera.res_x, self.camera.res_y, lodepng::ColorType::RGB, 8).expect("failed to write png");
    }
}
