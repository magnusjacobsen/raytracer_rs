use super::point::Point;
use super::ray::Ray;
use super::vector::Vector;

const ESCPAE_SCALAR: f64 = 0.000001;

pub struct HitPoint {
    pub time: f64, // the travel distance of the ray??
    pub normal: Vector,
    pub point: Point,
    pub escaped_point: Point,
}

impl HitPoint {
    pub fn new(time: f64, normal: Vector, ray: &Ray) -> Self {
        let point = ray.point_at_time(time);
        let escaped_point = point + normal * ESCPAE_SCALAR;
        let normal = if ray.direction * normal > 0.0 { 
            // not sure if this ever happens
            -normal 
        } else {
            normal
        };

        Self {time, normal, point, escaped_point}
    }
}