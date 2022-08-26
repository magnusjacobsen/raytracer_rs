//use rayon::prelude::*;
use lodepng;
use std::{path::Path, io::{stdout, Write}};

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
                .fold(ambient_color * 6.0, |color, light| {
                    let light_color = shape.material.bounce(&hit, &light, &ray);
                    let shadow_color = self.cast_shadow(&hit, light);
                    color + (light_color - shadow_color) * 1.2
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

        let mut buffer = vec![vec![color::BLACK; self.camera.res_x]; self.camera.res_y];

        let total_pixels = (self.camera.res_y * self.camera.res_x) as i32;
        let mut count = 0;
        let mut last= -1;

        for (x, y) in coords {
            count += 1;
            let rays = self.camera.create_rays(x, y);

            let percentage = (count * 100) / total_pixels;
            if percentage > last {
                last = percentage;
                let per_50 = percentage / 2;
                let progress = (0..per_50).map(|_| "▓").chain((0..(50 - per_50)).map(|_| "░")).collect::<Vec<_>>().join("");
                print!("\r{} {}%", progress, last);
                stdout().lock().flush().unwrap();
            };

            let color = self.cast(&rays[0]);
            buffer[self.camera.res_y - (y + 1)][x] = color;
        }
        println!("");

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
        let path = &Path::new("out/test_file.png");

        lodepng::encode_file(path, &image, self.camera.res_x, self.camera.res_y, lodepng::ColorType::RGB, 8).expect("failed to write png");
    }
}
