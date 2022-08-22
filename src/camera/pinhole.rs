use crate::core::{point::Point, vector::Vector, ray::Ray};

pub struct PinholeCamera {
    position: Point,
    //lookat: Point,
    //up: Vector,
    //zoom: f32,
    //width: f32,
    //height: f32,
    pub res_x: usize,
    pub res_y: usize,
    //w: Vector,
    u: Vector,
    v: Vector,
    w_zoom: Vector,
    pw: f32,
    ph: f32,
}

impl PinholeCamera {
    pub fn new(position: Point,
        lookat: Point,
        up: Vector,
        zoom: f32,
        width: f32,
        height: f32,
        res_x: usize,
        res_y: usize) -> Self {
        // field of view and orthonormal coordinate system
        let w = (position - lookat).normalize();
        let v = (up % w).normalize();
        let u = w % v;
        let w_zoom = w * zoom;
        let pw = width / res_x as f32;
        let ph = height / res_y as f32;

        Self {
            position, res_x, res_y, u, v, w_zoom, pw, ph,
        }
    }

    pub fn create_rays(&self, x: usize, y: usize) -> Vec<Ray> {
        //let samples = sampler.next_set(), let's just ignore sampling for now
        let px = self.pw * (x as i32 - (self.res_x as i32) / 2) as f32;
        let py = self.ph * (y as i32 - (self.res_y as i32) / 2) as f32;
        let direction = ((self.v * px) + (self.u * py) - self.w_zoom).normalize();

        vec![Ray::new(self.position, direction)]
    }
}