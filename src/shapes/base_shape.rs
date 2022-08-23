use crate::{materials::Material, implicit_surfaces::hit_function::HitFunction};

pub struct BaseShape {
    pub hit_function: HitFunction,
    pub material: Box<dyn Material>,
    // bounding_box: Pin<Box<dyn Fn() -> BoundingBox>>, not needed for implicit surfaces
}

impl BaseShape {
    pub fn new(hit_function: HitFunction, material: Box<dyn Material>) -> Self {
        Self {hit_function, material}
    }
}