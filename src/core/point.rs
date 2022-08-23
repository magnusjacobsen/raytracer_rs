use std::fmt::{Display, Formatter, Result};
use std::cmp::PartialEq;
use std::ops::{Sub, Mul, Div, Add};

use super::vector::Vector;

#[derive(Clone, Copy, Debug)]
pub struct Point {
    pub x: f64,
    pub y: f64,
    pub z: f64,
}

impl Point {
    pub fn new(x: f64, y: f64, z: f64) -> Self {
        Self {x, y, z}
    }

    pub fn zero() -> Self {
        Self {x: 0.0, y: 0.0, z: 0.0}
    }

    fn move_point(&self, v: &Vector) -> Self {
        Self::new(
            self.x + v.x, 
            self.y + v.y, 
            self.z + v.z
        )
    }

    fn subtract(&self, other: &Self) -> Vector {
        Vector::new(
            self.x - other.x,
            self.y - other.y,
            self.z - other.z
        )
    }

    pub fn distance(&self, other: &Self) -> Vector {
        other.subtract(self)
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
 
/*
    Operator overloading
*/

impl Add<Vector> for Point {
    type Output = Self;
    fn add(self, rhs: Vector) -> Self::Output {
        self.move_point(&rhs)
    }
}

impl Sub<Vector> for Point {
    type Output = Self;
    fn sub(self, rhs: Vector) -> Self::Output {
        self + (-rhs)
    }
}

impl Sub for Point {
    type Output = Vector;
    fn sub(self, rhs: Self) -> Vector {
        self.subtract(&rhs)
    }
}

impl Mul<f64> for Point {
    type Output = Self;
    fn mul(self, rhs: f64) -> Self::Output {
        Self::new(
            self.x * rhs,
            self.y * rhs,
            self.z * rhs,
        )
    }
}

impl Sub<f64> for Point {
    type Output = Self;
    fn sub(self, rhs: f64) -> Self::Output {
        Self::new(
            self.x - rhs,
            self.y - rhs,
            self.z - rhs,
        )
    }
}

impl Div<f64> for Point {
    type Output = Self;
    fn div(self, rhs: f64) -> Self::Output {
        Self::new(
            self.x / rhs,
            self.y / rhs,
            self.z / rhs,
        )
    }
}
