use super::point::Point;
use super::ray::Ray;
use super::vector::Vector;

const ESCPAE_SCALAR: f32 = 0.000001;

pub struct HitPoint {
    pub time: f32, // the travel distance of the ray??
    pub normal: Vector,
    pub point: Point,
    pub escaped_point: Point,
}

impl HitPoint {
    pub fn new(time: f32, normal: Vector, ray: &Ray) -> Self {
        let normal = if ray.direction * normal > 0.0 { 
            -normal 
        } else {
            normal
        };
        let point = ray.point_at_time(time);
        let escaped_point = point + normal * ESCPAE_SCALAR;

        Self {time, normal, point, escaped_point}
    }
}