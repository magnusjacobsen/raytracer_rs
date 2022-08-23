use crate::core::{point::Point, vector::Vector, ray::Ray};

pub struct PinholeCamera {
    position: Point,
    //lookat: Point,
    //up: Vector,
    //zoom: f64,
    //width: f64,
    //height: f64,
    pub res_x: usize,
    pub res_y: usize,
    //w: Vector,
    u: Vector,
    v: Vector,
    w_zoom: Vector,
    pw: f64,
    ph: f64,
}

impl PinholeCamera {
    pub fn new(position: Point,
        lookat: Point,
        up: Vector,
        zoom: f64,
        width: f64,
        height: f64,
        res_x: usize,
        res_y: usize) -> Self {
        // field of view and orthonormal coordinate system
        let w = (position - lookat).normalize();
        let v = (up % w).normalize();
        let u = w % v;
        let w_zoom = w * zoom;
        let pw = width / res_x as f64;
        let ph = height / res_y as f64;

        Self {
            position, res_x, res_y, u, v, w_zoom, pw, ph,
        }
    }

    pub fn create_rays(&self, x: usize, y: usize) -> Vec<Ray> {
        //let samples = sampler.next_set(), let's just ignore sampling for now
        let px = self.pw * (x as f64 - (self.res_x as f64) / 2.0);
        let py = self.ph * (y as f64 - (self.res_y as f64) / 2.0);
        let direction = self.v * px + self.u * py - self.w_zoom;

        vec![Ray::new(self.position, direction.normalize())]
    }
}