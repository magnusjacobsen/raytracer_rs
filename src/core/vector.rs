use std::fmt::{Display, Formatter, Result};
use std::cmp::PartialEq;

pub struct Vector {
    pub x: f32,
    pub y: f32,
    pub z: f32,
    _magnitude: f32,
}

impl Vector {
    pub fn new(x: f32, y: f32, z: f32) -> Self {
        Self {x, y, z, _magnitude: (x*x + y*y + z*z).sqrt()}
    }

    pub fn zero() -> Self {
        Self {x: 0.0, y: 0.0, z: 0.0, _magnitude: 0.0}
    }

    pub fn multiply_scalar(&self, s: f32) -> Self {
        Vector::new(self.x * s, self.y * s, self.z * s)
    }
}

impl Display for Vector {
    fn fmt(&self, fmt: &mut Formatter) -> Result {
        let str = format!("({}, {}, {})", self.x, self.y, self.z);
        fmt.write_str(&str)?;
        Ok(())
    }
}

impl PartialEq for Vector {
    fn eq(&self, other: &Vector) -> bool {
        self.x == other.x && self.y == other.y && self.z == other.z
    }
}

impl Eq for Vector {}

