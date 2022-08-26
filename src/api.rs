use crate::{lights::{point_light::PointLight, ambient_light::AmbientLight}, core::{color::{Color, self}, point::Point, vector::Vector}, shapes::base_shape::BaseShape, scene::Scene, implicit_surfaces::solving, camera::pinhole::PinholeCamera, render::Render, materials::{phong::Phong, matte::Matte}, bx};

fn make_scene(shape: BaseShape) -> Scene {
    let light = make_light(color::WHITE, 0.5, Point::new(4.0, 2.0, 4.0));
    let light2 = make_light(color::WHITE, 0.5, Point::new(-4.0, 2.0, 4.0));
    let lights = vec![light, light2];
    let ambient_light = make_ambient_light(color::WHITE, 0.1);

    _make_scene(
        vec![shape], 
        lights, 
        ambient_light, 
        0
    )
}

fn make_camera(square: bool) -> PinholeCamera {
    PinholeCamera::new(
        Point::new(0.0, 0.0, 4.0),
        Point::new(0.0, 0.0, 0.0),
        Vector::new(0.0, 1.0, 0.0),
        2.0,
        4.0,
        if square {4.0} else {3.0},
        if square {500} else {1024},
        if square {500} else {768}
    )
}

pub fn _sphere1(r: f64, _num_samples: i32) -> Render {
    let main_color = color::AQUA;
    let white = color::WHITE;
    let material = Phong::new(main_color, 0.2, main_color, 0.8, white, 0.7, 100);

    let r_r = r * r;

    let s = solving::make_implicit(format!("x^2 + y^2 + z^2 - {r_r}"), bx!(material));
    let camera = make_camera(false);
    let scene = make_scene(s);
    Render::new(scene, camera)
}

pub fn _torus(r: f64, rr: f64) -> Render {
    let c = color::BLUE;
    let material = Matte::new(c, 1.0, c, 1.0);
    let s = solving::make_implicit(format!(
        "(((x^2 + y^2)_2 - 1.5)^2 + z^2)_2 - 0.5"
    ), bx!(material));
    let camera = make_camera(true);

    let scene = make_scene(s);
    Render::new(scene, camera)
}

pub fn _torus_phong(r: f64, rr: f64) -> Render {
    let main_color = color::FUCHSIA;
    let white = color::WHITE;
    let material = Phong::new(main_color, 0.2, main_color, 0.8, white, 0.7, 100);
    let s = solving::make_implicit(format!("(((x^2 + y^2)_2 - {r})^2 + z^2)_2 - {rr}"), bx!(material));
    let camera = make_camera(true);

    let scene = make_scene(s);
    Render::new(scene, camera)
}

pub fn _torus2(r: f64, small_r: f64) -> Render {
    let rs1 = format!("({r}^2 + {small_r}^2)");
    let rs2 = format!("({r}^2 - {small_r}^2)");
    let sx = format!("x^4 + 2x^2*y^2 + 2x^2*z^2 - 2*{rs1}*x^2");
    let sy = format!("y^4 + 2y^2*z^2 + 2*{rs2}*y^2");
    let sz = format!("z^4 - 2*{rs1}*z^2");
    let sc = format!("{rs2}^2");
    let eqn = format!("{sx} + {sy} + {sz} + {sc}"); 

    let material = Matte::new(color::BLUE, 1.0, color::BLUE, 1.0);
    let s = solving::make_implicit(eqn, bx!(material));
    let scene = make_scene(s);
    let camera = PinholeCamera::new(
        Point::new(0.0, 4.0, 0.0),
        Point::new(0.0, 0.0, 0.0),
        Vector::new(0.0, 0.0, 1.0),
        2.0,
        4.0,
        4.0,
        500,
        500
    );

    Render::new(scene, camera)
}

fn make_light(c: Color, i: f64, p: Point) -> PointLight {
    PointLight::new(c, i, p)
}

fn _make_scene(shapes: Vec<BaseShape>, lights: Vec<PointLight>, ambient: AmbientLight, max_bounces: i32) -> Scene {
    Scene::new(shapes, lights, ambient, max_bounces)
}

fn make_ambient_light(color: Color, intensity: f64) -> AmbientLight {
    AmbientLight::new(color, intensity)
}