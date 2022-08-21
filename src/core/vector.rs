use std::fmt::{Display, Formatter, Result};
use std::cmp::PartialEq;

#[derive(Clone, PartialOrd)]
pub struct Vector {
    pub x: f32,
    pub y: f32,
    pub z: f32,
    pub magnitude: f32,
}

impl Vector {
    pub fn new(x: f32, y: f32, z: f32) -> Self {
        Self {x, y, z, magnitude: (x*x + y*y + z*z).sqrt()}
    }

    pub fn zero() -> Self {
        Self {x: 0.0, y: 0.0, z: 0.0, magnitude: 0.0}
    }

    pub fn multiply_scalar(&self, s: f32) -> Self {
        Self::new(self.x * s, self.y * s, self.z * s)
    }

    pub fn normalize(&self) -> Self {
        match self.magnitude {
            len if len == 0.0   => self.clone(),
            len                 => 
                Self::new(self.x / len, self.y / len, self.z / len),
        }
    }

    pub fn cross_product(&self, other: &Self) -> Self {
        Self::new(
            self.y * other.z - self.z * other.y,
            self.z * other.x - self.x - other.z,
            self.x * other.y - self.y - other.x,
        )
    }

    pub fn dot_product(&self, other: &Self) -> f32 { 
        self.x * other.x +
        self.y * other.y +
        self.z * other.z
    }

    pub fn add(&self, other: &Self) -> Self {
        Self::new(
            self.x + other.x,
            self.y + other.x,
            self.z + other.z,
        )
    }

    pub fn subtract(&self, other: &Self) -> Self {
        Self::new(
            self.x - other.x,
            self.y - other.y,
            self.z - other.z,
        )
    }

    pub fn negate(&self) -> Self {
        Self::new(
            -self.x,
            -self.y,
            -self.z,
        )
    }

    pub fn add_f32(&self, f: f32) -> Self {
        Self::new(
            self.x + f,
            self.y + f,
            self.z + f,
        )
    }

    pub fn powi(&self, exponent: i32) -> Self {
        Self::new(
            self.x.powi(exponent),
            self.y.powi(exponent),
            self.z.powi(exponent),
        )
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

