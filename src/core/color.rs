#[derive(Debug, Clone, Copy)]
pub struct Color {
    pub r: f32,
    pub g: f32,
    pub b: f32,
}

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
    pub fn new(r: f32, g: f32, b: f32) -> Self {
        if r < 0.0 || g < 0.0 || b < 0.0 {
            panic!("Color::new: colors can not be negative!");
        }

        Self {r, g, b}
    }

    pub fn scale(&self, s: f32) -> Color {
        if s <= 0.0 {
            WHITE
        } else {
            Color::new(self.r * s, self.g * s, self.b * s)
        }
    }

    pub fn merge(&self, w: f32) -> Color {
        if w < 0.0 || w > 1.0 {
            panic!("Color::merge: w i to small or too big");
        } else {
            let dw = 1.0 - w;
            Color::new(w * self.r + dw * self.r, w * self.g + dw * self.b, w * self.b + dw * self.b)
        }
    }

    pub fn average(&self) -> f32 {
        (self.r + self.g + self.b) * 3.0
    }

    pub fn add(&self, other: Color) -> Color {
        Color::new(
            (self.r + other.r).min(1.0), 
            (self.g + other.g).min(1.0), 
            (self.b + other.b).min(1.0),
        )
    }

    pub fn subtract(&self, other: Color) -> Color {
        Color::new(
            (self.r - other.r).max(0.0),
            (self.g - other.g).max(0.0),
            (self.b - other.b).max(0.0),
        )
    }

    pub fn multiply(&self, other: Color) -> Color {
        Color::new(
            self.r * other.r,
            self.g * other.g,
            self.b * other.b,
        )
    }

    pub fn scale_division(&self, s: f32) -> Color {
        self.scale(1.0 / s)
    }

    pub fn to_u8_vec(&self) -> Vec<u8> {
        vec![
            (self.r * 255.0) as u8,
            (self.g * 255.0) as u8,
            (self.b * 255.0) as u8,
        ]
    }
}