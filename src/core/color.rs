use std::ops::{Add, Sub, Mul, Div};

#[derive(Debug, Clone, Copy)]
pub struct Color {
    pub r: f64,
    pub g: f64,
    pub b: f64,
}

// predefined colors
pub const WHITE: Color = Color {r: 1.0, g: 1.0, b: 1.0};
pub const ZERO: Color = Color {r: 0.0, g: 0.0, b: 0.0};
pub const BLACK: Color = Color {r: 0.0, g: 0.0, b: 0.0};
pub const RED: Color = Color {r: 1.0, g: 0.0, b: 0.0};
pub const GREEN: Color = Color {r: 0.0, g: 1.0, b: 0.0};
pub const BLUE: Color = Color {r: 0.0, g: 0.0, b: 1.0};
pub const YELLOW: Color = Color {r: 1.0, g: 1.0, b: 0.0};
pub const AQUA: Color = Color {r: 0.0, g: 1.0, b: 1.0};
pub const FUCHSIA: Color = Color {r: 1.0, g: 0.0, b: 1.0};

impl Color {
    pub fn new(r: f64, g: f64, b: f64) -> Self {
        if r < 0.0 || g < 0.0 || b < 0.0 {
            panic!("Color::new: colors can not be negative!");
        }

        Self {r, g, b}
    }

    fn scale(&self, s: f64) -> Self {
        if s <= 0.0 {
            WHITE
        } else {
            Self::new(
                self.r * s, 
                self.g * s, 
                self.b * s,
            )
        }
    }

    /*fn merge(&self, w: f64) -> Color {
        if w < 0.0 || w > 1.0 {
            panic!("Color::merge: w i to small or too big");
        } else {
            let dw = 1.0 - w;
            Color::new(w * self.r + dw * self.r, w * self.g + dw * self.b, w * self.b + dw * self.b)
        }
    }*/

    /*fn average(&self) -> f64 {
        (self.r + self.g + self.b) * 3.0
    }*/

    pub fn to_u8_vec(&self) -> Vec<u8> {
        vec![
            (self.r * 255.0) as u8,
            (self.g * 255.0) as u8,
            (self.b * 255.0) as u8,
        ]
    }


}

/*
    Operator overloading
*/
impl Add for Color {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self::new(
            self.r + rhs.r,
            self.g + rhs.g,
            self.b + rhs.b,
        )
    }
}

impl Sub for Color {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        Self::new(
            (self.r - rhs.r).max(0.0),
            (self.g - rhs.g).max(0.0),
            (self.b - rhs.b).max(0.0),
        )
    }
}

impl Mul for Color {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        Self::new(
            self.r * rhs.r,
            self.g * rhs.g,
            self.b * rhs.b,
        )
    }
}

impl Mul<f64> for Color {
    type Output = Self;
    fn mul(self, rhs: f64) -> Self::Output {
        self.scale(rhs)
    }
}

impl Div<f64> for Color {
    type Output = Self;
    fn div(self, rhs: f64) -> Self::Output {
        self.scale(1.0 / rhs)
    }
}

impl Div<i32> for Color {
    type Output = Self;
    fn div(self, rhs: i32) -> Self::Output {
        self.scale(1.0 / rhs as f64)
    }
}