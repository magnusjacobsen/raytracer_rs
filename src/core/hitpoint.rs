use super::ray::Ray;
use super::vector::Vector;
use super::shape::Shape;
use super::material::Material;

pub struct HitPoint {
    ray: Ray,
    time: f32,
    normal: Vector,
    material: Material,
    shape: Shape,
    u: f32,
    v: f32,
    did_hit: bool,
}

impl HitPoint {
    pub fn new(ray: Ray, time: f32, normal: Vector, material: Material, shape: Shape, u: f32, v: f32, did_hit: bool) -> Self {
        Self { ray, time, normal, material, shape, u, v, did_hit }
    }

    pub fn point(&self) {
        self.ray.
    }
}