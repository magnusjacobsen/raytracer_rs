use crate::{lights::{point_light::PointLight, ambient_light::AmbientLight}, core::{color::{Color, self}, point::Point, vector::Vector}, shapes::base_shape::BaseShape, scene::Scene, implicit_surfaces::implicit_surfaces, camera::pinhole::PinholeCamera, render::Render, materials::phong::Phong, bx};

pub fn make_scene(shape: BaseShape) -> Scene {
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

pub fn sphere1(r: f32, _num_samples: i32) -> Render {
    let main_color = color::FUCHSIA;
    let white = color::WHITE;
    let material = Phong::new(main_color, 0.2, main_color, 0.8, white, 0.7, 100);
    // mkPhongMaterial aqua 0.2 aqua 0.8 white 0.7 100

    let s = implicit_surfaces::make_implicit(
        format!("x^2 + y^2 + z^2 - {r}"), bx!(material));
    let camera = PinholeCamera::new(
        Point::new(0.0, 0.0, 4.0),
        Point::new(0.0, 0.0, 0.0),
        Vector::new(0.0, 1.0, 0.0),
        2.0,
        4.0,
        3.0,
        1024,
        768
    );
    let scene = make_scene(s);
    Render::new(scene, camera)
}

fn make_light(c: Color, i: f32, p: Point) -> PointLight {
    PointLight::new(c, i, p)
}

fn _make_scene(shapes: Vec<BaseShape>, lights: Vec<PointLight>, ambient: AmbientLight, max_bounces: i32) -> Scene {
    Scene::new(shapes, lights, ambient, max_bounces)
}

fn make_ambient_light(color: Color, intensity: f32) -> AmbientLight {
    AmbientLight::new(color, intensity)
}