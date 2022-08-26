#![feature(test, type_alias_impl_trait)]
pub mod core;
pub mod implicit_surfaces;
pub mod util;
pub mod lights;
pub mod materials;
pub mod shapes;
pub mod camera;
pub mod sampling;

pub mod api;
pub mod scene;
pub mod render;

fn main() {
    //let render = api::_sphere1(1.0, 4);
    //let render = api::_torus(1.5, 0.5);
    let render = api::_torus2(1.5, 0.5);
    //let render = api::_torus(0.2, 1.2);
    render.save_image("hello world");
}
