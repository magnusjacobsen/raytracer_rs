use crate::{shapes::base_shape::BaseShape, lights::{ambient_light::AmbientLight, point_light::PointLight}, core::color::{self, Color}};

pub struct Scene {
    pub shapes: Vec<BaseShape>,
    pub lights: Vec<PointLight>,
    pub ambient: AmbientLight,
    pub max_bounces: i32,
    pub background: Color
}

impl Scene {
    pub fn new(shapes: Vec<BaseShape>, lights: Vec<PointLight>, ambient: AmbientLight, max_bounces: i32) -> Self {
        Self {shapes, lights, ambient, max_bounces, background: color::BLACK}
    }
}