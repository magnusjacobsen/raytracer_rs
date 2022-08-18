use std::fmt::{Display, Formatter, Result};
use std::cmp::PartialEq;

use super::vector::Vector;

pub struct Point {
    x: f32,
    y: f32,
    z: f32,
}

impl Point {
    pub fn new(x: f32, y: f32, z: f32) -> Self {
        Self {x, y, z}
    }

    pub fn zero() -> Self {
        Self {x: 0.0, y: 0.0, z: 0.0}
    }

    pub fn move_point(&self, v: Vector) -> Self {
        Point::new(self.x + v.x, self.y + v.y, self.z + v.z)
    }
}

impl Display for Point {
    fn fmt(&self, fmt: &mut Formatter) -> Result {
        let str = format!("({}, {}, {})", self.x, self.y, self.z);
        fmt.write_str(&str)?;
        Ok(())
    }
}

impl PartialEq for Point {
    fn eq(&self, other: &Point) -> bool {
        self.x == other.x && self.y == other.y && self.z == other.z
    }
}

impl Eq for Point {}

