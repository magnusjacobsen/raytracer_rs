use crate::core::{ray::Ray, hit_point::HitPoint, vector::Vector};

use super::{uni_poly::{IntSimpleExpr, self}, expr::Expr, solving};

pub struct HitFunction {
    a: IntSimpleExpr,
    b: IntSimpleExpr,
    c: IntSimpleExpr,
    poly_vec: Vec<(i32, IntSimpleExpr)>,
    pdx: Expr,
    pdy: Expr,
    pdz: Expr,
    degree: i32,
}

/*
    a simple HitFunction, for any degree (expect 0) polynomial.

    Input:
        * (i32, IntSimpleExpr) vec, an optimized form of a polynomial,
        * derivative with respect to x,
        * derivative with respect to y,
        * derivative with respect to z.
    */
impl HitFunction {
    pub fn new(poly_vec: Vec<(i32, IntSimpleExpr)>, pdx: Expr, pdy: Expr, pdz: Expr) -> Self {
        let degree = poly_vec.last().expect("should not be empty").0;
        match degree {
            0 => panic!("HitFunction: get me outta here"),
            1 => {
                let mut poly = poly_vec.clone();
                let a = if !poly.is_empty() && poly.last().unwrap().0 == 1 {
                    poly.pop().unwrap().1
                } else {
                    vec![vec![]]
                };
                let b = if !poly.is_empty() && poly.last().unwrap().0 == 0 {
                    poly.pop().unwrap().1
                } else {
                    vec![vec![]]
                };

                Self {a, b, c: vec![vec![]], poly_vec, pdx, pdy, pdz, degree}
            },
            2 => {
                println!("poly_vec: {:?}\n", poly_vec);
                let mut poly = poly_vec.clone();
                let a = poly.pop().unwrap().1; // we know it is degree 2, otherwise we wouldn't be here
                let b = if !poly.is_empty() && poly.last().unwrap().0 == 1 {
                    poly.pop().unwrap().1
                } else {
                    vec![vec![]]
                };
                let c = if !poly.is_empty() && poly.last().unwrap().0 == 0 {
                    poly.pop().unwrap().1
                } else {
                    vec![vec![]]
                };

                Self {a, b, c, poly_vec, pdx, pdy, pdz, degree}
            },
            _ => {
                Self {a: vec![vec![]], b: vec![vec![]], c: vec![vec![]], poly_vec, pdx, pdy, pdz, degree}
            }
        }
    }

    fn first_degree(&self, ray: &Ray) -> Option<HitPoint> {
        let ray_values = solving::get_ray_values(&ray);
        let a = uni_poly::solve_int_simple_expr(&self.a, ray_values);
        let b = uni_poly::solve_int_simple_expr(&self.b, ray_values);

        let t = (-b) / a;
        if t < 0.0 {
            None
        } else {
            let point_time = ray.point_at_time(t);
            let normal = solving::normal_vector(&point_time, &self.pdx, &self.pdy, &self.pdz);
            let hit = HitPoint::new(t, normal, &ray);
            Some(hit)
        }
    }

    fn second_degree(&self, ray: &Ray, print: bool) -> Option<HitPoint> {
        let ray_values = solving::get_ray_values(&ray);
        let a = uni_poly::solve_int_simple_expr(&self.a, ray_values);
        let b = uni_poly::solve_int_simple_expr(&self.b, ray_values);
        let c = uni_poly::solve_int_simple_expr(&self.c, ray_values);
        let dis = solving::discriminant(a, b, c);
        
        if print {
            println!("ray_values: {:?}", ray_values);
            println!("a: {:?}", self.a);
            println!("b: {:?}", self.b);
            println!("c: {:?}", self.c);
            println!("a: {a}, b: {b}, c: {c}, dis: {dis}\n");
        }
        if dis < 0.0 {
            None
        } else {

            let distances = solving::get_distances(a, b, dis);
            let ts = distances.iter().filter(|x| **x >= 0.0).collect::<Vec<_>>();
            if ts.is_empty() {
                None
            } else {
                println!("we god a hit");
                let min_t = if ts.len() == 1 || ts[0] < ts[1] { ts[0] } else { ts[1] };
                let point_time = ray.point_at_time(*min_t);
                let normal = solving::normal_vector(&point_time, &self.pdx, &self.pdy, &self.pdz);
                let hit = HitPoint::new(*min_t, normal, &ray);
                Some(hit)
            }
        }
    }

    /*
        Uses a sturm sequence chain to get an initial guess, which is then passed to the newtonRaphson function
        if the newtonRaphson result is outside the initial guess interval (where we know the smallest real root exists),

        we try again. This is stopped when a good approximation of the smallest root is found, no result has been found, or we have done the entire operation 5 times.
    */
    fn higher_degree(&self, ray: &Ray) -> Option<HitPoint> {
        let mut lo = 0.0;
        let mut hi = 100.0;
        let mut max_depth = 15;
        let newton_runs = 5;
        let iterations = 5;
        
        let ray_values = solving::get_ray_values(&ray);
        // now that we now the ray values, we can turn our multivariable polynomial into a univariate one
        let up = uni_poly::to_uni_poly(&self.poly_vec, ray_values);
        let derivative = uni_poly::uni_poly_derivative(&up);

        let sturm_seq = uni_poly::sturm_seq(&up, &derivative);
        //println!("up: {:?}", up);
        //println!("derivative: {:?}", derivative);
        //println!("sturm_seq: {:?}\n", sturm_seq);
        for _ in 0..iterations {
            if let Some((int_lo, int_hi)) = uni_poly::get_interval(&sturm_seq, lo, hi, max_depth) {
                lo = int_lo;
                hi = int_hi;
                let mid = lo + (hi - lo) / 2.0;
                if let Some(t)  = solving::newton_raphson(&up, &derivative, newton_runs, mid) {
                    if t < lo { 
                        max_depth = 5;
                        lo = mid;
                        continue;
                    } else if t > hi {
                        max_depth = 5;
                        hi = mid;
                        continue;
                    } else {
                        let point_time = ray.point_at_time(t);
                        let normal = solving::normal_vector(&point_time, &self.pdx, &self.pdy, &self.pdz);
                        let hit = HitPoint::new(t, normal, &ray);
                        return Some(hit);
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        None
    }

    pub fn run(&self, ray: &Ray, print: bool) -> Option<HitPoint> {
        match self.degree {
            0 => panic!("HitFunction::run: should not be here!"),
            1 => self.first_degree(ray),
            2 => self.second_degree(ray, print),
            _ => self.higher_degree(ray),
        }
    }
}