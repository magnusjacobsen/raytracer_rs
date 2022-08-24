#![feature(test, type_alias_impl_trait)]
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
    /*let torus = format!("(((x^2 + y^2)_2 - 1.0)^2 + z^2)_2 - 1.0");
    let expr = parse_string(torus);
    let poly = expr_to_poly(expr.clone(), "t".into());
    let poly2 = expr_to_poly(subst_with_ray(&expr), "t".into());
    pretty_print("t", &poly);
    pretty_print("t", &poly2); */
    let render = api::_sphere1(1.0, 4);
    //let render = api::_torus(1.5, 0.5);
    //let render = api::_torus(0.2, 1.2);
    render.save_image("hello world");
}
