use super::point::Point;
use super::vector::Vector;

use std::cmp::PartialEq;

#[derive(Debug)]
pub struct Ray {
    pub origin: Point,
    pub direction: Vector,
}

impl Ray {
    pub fn new(origin: Point, direction: Vector) -> Self {
        Self {origin, direction}
    }

    pub fn zero() -> Self {
        Self {origin: Point::zero(), direction: Vector::zero()}
    }

    pub fn point_at_time(&self, t: f64) -> Point {
        self.origin + self.direction * t
    }

    /*pub fn time_at_point(&self, p: &Point) -> f64 {
        (*p - self.origin).z / self.direction.z
    }*/
}

impl PartialEq for Ray {
    fn eq(&self, other: &Ray) -> bool {
        self.origin == other.origin && self.direction == other.direction
    }
}

impl Eq for Ray {}