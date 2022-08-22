#![feature(test, type_alias_impl_trait)]

use api::sphere1;
pub mod core;
pub mod implicit_surfaces;
pub mod util;
pub mod lights;
pub mod materials;
pub mod shapes;
pub mod camera;

pub mod api;
pub mod scene;
pub mod render;

fn main() {
    let render = api::torus(1.0, 1.0);
    render.save_image("hello world");
}
