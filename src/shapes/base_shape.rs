use std::pin::Pin;

use crate::{materials::Material, core::{point::Point, ray::Ray, hitpoint::HitPoint}};

pub type InsideFunction = Pin<Box<dyn Fn(&Point) -> bool>>;
pub type HitFunction = Pin<Box<dyn Fn(&Ray) -> Option<HitPoint>>>;

pub struct BaseShape {
    pub is_inside: InsideFunction,
    pub hit_function: HitFunction,
    pub material: Box<dyn Material>,
    // bounding_box: Pin<Box<dyn Fn() -> BoundingBox>>, not needed for implicit surfaces
}

impl BaseShape {
    pub fn new(is_inside: InsideFunction, hit_function: HitFunction, material: Box<dyn Material>) -> Self {
        Self {is_inside, hit_function, material}
    }
}