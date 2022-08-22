use super::point::Point;
use super::vector::Vector;

use std::cmp::PartialEq;

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

    pub fn point_at_time(&self, t: f32) -> Point {
        self.origin + self.direction * t
    }
}

impl PartialEq for Ray {
    fn eq(&self, other: &Ray) -> bool {
        self.origin == other.origin && self.direction == other.direction
    }
}

impl Eq for Ray {}