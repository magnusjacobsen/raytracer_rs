use std::fmt::{Display, Formatter, Result};
use std::cmp::PartialEq;
use std::ops::{Add, Sub, Mul, Div, Neg, Rem};

use super::point::Point;

#[derive(Clone, Copy, PartialOrd)]
pub struct Vector {
    pub x: f64,
    pub y: f64,
    pub z: f64,
    pub magnitude: f64,
}

impl Vector {
    pub fn new(x: f64, y: f64, z: f64) -> Self {
        Self {x, y, z, magnitude: (x*x + y*y + z*z).sqrt()}
    }

    pub fn zero() -> Self {
        Self {x: 0.0, y: 0.0, z: 0.0, magnitude: 0.0}
    }

    fn multiply_scalar(&self, s: f64) -> Self {
        Self::new(self.x * s, self.y * s, self.z * s)
    }

    pub fn normalize(&self) -> Self {
        match self.magnitude {
            len if len == 0.0   => self.clone(),
            len                 => 
                Self::new(
                    self.x / len, 
                    self.y / len, 
                    self.z / len,
                ),
        }
    }

    fn cross_product(&self, other: &Self) -> Self {
        Self::new(
            self.y * other.z - self.z * other.y,
            self.z * other.x - self.x - other.z,
            self.x * other.y - self.y - other.x,
        )
    }

    fn dot_product(&self, other: &Self) -> f64 { 
        self.x * other.x +
        self.y * other.y +
        self.z * other.z
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

/*
    Operator overloading!
*/
impl Sub<Point> for Vector {
    type Output = Self;
    fn sub(self, rhs: Point) -> Self::Output {
        Self::new(
            self.x - rhs.x,
            self.y - rhs.y,
            self.z - rhs.z,
        )
    }
}

impl Neg for Vector {
    type Output = Self;
    fn neg(self) -> Self::Output {
        Self::new(
            -self.x,
            -self.y,
            -self.z,
        )
    }
}

impl Add for Vector {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self::new(
            self.x + rhs.x,
            self.y + rhs.x,
            self.z + rhs.z,
        )
    }
}

impl Add<f64> for Vector {
    type Output = Self;
    fn add(self, rhs: f64) -> Self::Output {
        Self::new(
            self.x + rhs,
            self.y + rhs,
            self.z + rhs,
        )
    }
}

impl Sub for Vector {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        Self::new(
            self.x - rhs.x,
            self.y - rhs.y,
            self.z - rhs.z,
        )
    }
}

impl Mul<f64> for Vector {
    type Output = Self;
    fn mul(self, rhs: f64) -> Self::Output {
        self.multiply_scalar(rhs)
    }
}

impl Mul for Vector {
    type Output = f64;
    fn mul(self, rhs: Self) -> f64 {
        self.dot_product(&rhs)
    }
}

impl Rem for Vector {
    type Output = Self;
    fn rem(self, rhs: Self) -> Self::Output {
        self.cross_product(&rhs)
    }
}

impl Div<f64> for Vector {
    type Output = Self;
    fn div(self, rhs: f64) -> Self::Output {
        self.multiply_scalar(1.0 / rhs)
    }
}

impl Div for Vector {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        Self::new(
            self.x / rhs.x,
            self.y / rhs.y,
            self.z / rhs.z,
        )
    }
}