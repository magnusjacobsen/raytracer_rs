use crate::bx;
use crate::core::hitpoint::HitPoint;
use crate::core::point::Point;
use crate::core::ray::Ray;
use crate::materials::Material;
use crate::shapes::base_shape::{HitFunction, BaseShape};
use crate::core::vector::Vector;
use super::expr::{self, Expr};
use super::poly::{self};
use super::uni_poly::{self, UniPoly, IntSimpleExpr};

// const values used in the newton_raphson function
const TOLERANCE: f32 = 0.000001; // 10^-5
const EPSILON: f32 = 0.00000000001; // 10^-10

/*
    Substitutes ray variables (p + t * d) into an Expr, that represents an implicit surface
*/
fn subst_with_ray(exp: &Expr) -> Expr {
    let ex = Expr::Add(
        bx!(Expr::Var("ox".into())),
        bx!(Expr::Mul(
            bx!(Expr::Var("t".into())),
            bx!(Expr::Var("dx".into()))
        ))
    );
    let ey = Expr::Add(
        bx!(Expr::Var("oy".into())),
        bx!(Expr::Mul(
            bx!(Expr::Var("t".into())),
            bx!(Expr::Var("dy".into()))
        ))
    );
    let ez = Expr::Add(
        bx!(Expr::Var("oz".into())),
        bx!(Expr::Mul(
            bx!(Expr::Var("t".into())),
            bx!(Expr::Var("dz".into()))
        ))
    );
    let subbed_x = expr::subst(exp, "x", &ex);
    let subbed_xy = expr::subst(&subbed_x, "y", &ey);
    expr::subst(&subbed_xy, "z", &ez)
}

/*
    Returns a partial derivative of an Expr, with respect to var variable
*/
fn partial_derivative(exp: &Expr, var: String) -> Expr {
    fn inner(exp: &Expr, var: &str) -> Expr {
        match exp {
            Expr::Num(_) => // case 1
                Expr::Num(0.0),
            Expr::Var(x) => 
                if x != var { // case 1
                    Expr::Num(0.0) 
                } else { // case 2
                    Expr::Num(1.0)
                },
            Expr::Exponent(e1, n) => // case 6
                Expr::Mul( 
                    bx!(inner(e1, var)),
                    bx!(Expr::Mul(
                        bx!(Expr::Num(*n as f32)),
                        bx!(Expr::Exponent(e1.clone(), n - 1))
                    ))
                ),
            Expr::Add(e1, e2) => // case 3
                Expr::Add(bx!(inner(e1, var)), bx!(inner(e2, var))),
            Expr::Mul(e1, e2) => // case 4
                Expr::Add( 
                    bx!(Expr::Mul(
                        bx!(inner(e1, var)),
                        e2.clone()
                    )),
                    bx!(Expr::Mul(
                        bx!(inner(e2, var)),
                        e1.clone()
                    ))
                ),
            Expr::Div(e1, e2) => // case 5
                Expr::Div(
                    bx!(Expr::Add(
                        bx!(Expr::Mul(
                            e2.clone(),
                            bx!(inner(e1, var))
                        )),
                        bx!(Expr::Mul(
                            bx!(Expr::Num(-1.0)),
                            bx!(Expr::Mul(
                                e1.clone(),
                                bx!(inner(e2, var))
                            ))
                        ))
                    )),
                    bx!(Expr::Exponent(e2.clone(), 2))
                ),
            Expr::Root(e1, n) => // case 7
                Expr::Div(
                    bx!(inner(e1, var)),
                    bx!(Expr::Mul(
                        bx!(Expr::Num(*n as f32)),
                        bx!(Expr::Exponent(
                            bx!(Expr::Root(e1.clone(), *n)), 
                            n - 1
                        ))
                    ))
                )
        }
    }

    expr::reduce_expr(inner(exp, var.as_str()))
}

/*
    Returns a vector, based on the intial implicit shape equation, and partially derived with respect to x, y, and z from the hitpoint.

    Thou shall not be simplified!
*/
fn normal_vector(p: &Point, dx: &Expr, dy: &Expr, dz: &Expr) -> Vector {
    let x = expr::solve_expr(dx, p);
    let y = expr::solve_expr(dy, p);
    let z = expr::solve_expr(dz, p);
    Vector::new(x, y, z).normalize()
}

/*
    Calculates a discriminant from the quadratic equation values of a, b, and c.
*/
fn discriminant(a: f32, b: f32, c: f32) -> f32 {
    b.powi(2) - 4.0 * a * c
}

/*
    Returns two t-values in a vec, when given a quadratic equation's a, b, and discrimant.

    Requires that the discrimant is not negative (in that case there is no real solution)
*/
fn get_distances(a: f32, b: f32, dis: f32) -> Vec<f32> {
    let sres = dis.sqrt();
    let res_pos = (-b + sres) / (2.0 * a);
    let res_neg = (-b - sres) / (2.0 * a);
    vec![res_pos, res_neg]
}

/*
    Create a f32 array with values from the ray
*/
fn get_ray_values(ray: &Ray) -> [f32; 6] {
    [
        ray.origin.x, // ox
        ray.origin.y, // oy
        ray.origin.z, // oz
        ray.direction.x, // dx
        ray.direction.y, // dy
        ray.direction.z, // dz
    ]
}

/*
    Root-finding algorithm algorithm that, given an initial guess, converges on a better approximation

    Runs until runs reaches 0, or when a result has been found, or no result is possible.

    Based on the pseudo code given here: https://en.wikipedia.org/wiki/Newton%27s_method#Pseudocode
      but adapted to a functional, immutable, approach.
*/
fn newton_raphson(f: &UniPoly, df: &UniPoly, runs: usize, guess: f32) -> Option<f32> {
    if runs == 0 {
        None
    } else {
        let y = uni_poly::solve_uni_poly(&f, guess);
        let dy = uni_poly::solve_uni_poly(&df, guess);
        if dy.abs() < EPSILON {
            None
        } else {
            let new_guess = guess - (y / dy);
            if (new_guess - guess).abs() <= (TOLERANCE * new_guess.abs()) {
                Some(new_guess)
            } else {
                newton_raphson(f, df, runs - 1, new_guess)
            }
        }
    }
}

/*
    Returns a simple HitFunction, for a first degree polynomial.

    Input:
        * (i32, IntSimpleExpr) vec, an optimized form of a polynomial,
        * derivative with respect to x,
        * derivative with respect to y,
        * derivative with respect to z.
*/
fn hit_function_first_degree(mut poly: Vec<(i32, IntSimpleExpr)>, pdx: Expr, pdy: Expr, pdz: Expr) -> HitFunction {
    let a_ise = if !poly.is_empty() && poly[0].0 == 1 {
        let res = poly[0].1.clone();
        poly.remove(0);
        res
    } else {
        vec![vec![]]
    };
    let b_ise = if !poly.is_empty() && poly[0].0 == 0 {
        poly[0].1.clone()
    } else {
        vec![vec![]]
    };

    // the returned hit function
    let hit_function = move |ray: &Ray| {
        let ray_values = get_ray_values(&ray);
        let a = uni_poly::solve_int_simple_expr(&a_ise, ray_values);
        let b = uni_poly::solve_int_simple_expr(&b_ise, ray_values);
        let t = (-b) / a;
        if t < 0.0 {
            None
        } else {
            let point_time = ray.point_at_time(t);
            let normal = normal_vector(&point_time, &pdx, &pdy, &pdz);
            let hit = HitPoint::new(t, normal, &ray);
            Some(hit)
        }
    };

    Box::pin(hit_function)
}

/*
    Returns a simple HitFunction, for a second degree polynomial.

    Input:
        * (i32, IntSimpleExpr) vec, an optimized form of a polynomial,
        * derivative with respect to x,
        * derivative with respect to y,
        * derivative with respect to z.
*/
fn hit_function_second_degree(mut poly: Vec<(i32, IntSimpleExpr)>, pdx: Expr, pdy: Expr, pdz: Expr) -> HitFunction {
    let a_ise = poly[0].1.clone(); // we know it is degree 2, otherwise we wouldn't be here
    poly.remove(0);
    let b_ise = if !poly.is_empty() && poly[0].0 == 1 {
        let res = poly[0].1.clone();
        poly.remove(0);
        res
    } else {
        vec![vec![]]
    };
    let c_ise = if !poly.is_empty() && poly[0].0 == 0 {
        poly[0].1.clone()
    } else {
        vec![vec![]]
    };

    // the hit function that is returned
    let hit_function = move |ray: &Ray| {
        let ray_values = get_ray_values(&ray);
        let a = uni_poly::solve_int_simple_expr(&a_ise, ray_values);
        let b = uni_poly::solve_int_simple_expr(&b_ise, ray_values);
        let c = uni_poly::solve_int_simple_expr(&c_ise, ray_values);
        let dis = discriminant(a, b, c);
        if dis < 0.0 {
            None
        } else {
            let distances = get_distances(a, b, dis);
            let ts = distances.iter().filter(|x| **x >= 0.0).collect::<Vec<_>>();
            if ts.is_empty() {
                None
            } else {
                let min_t = if ts[0] < ts[1] { *ts[0] } else { *ts[1] };
                let point_time = ray.point_at_time(min_t);
                let normal = normal_vector(&point_time, &pdx, &pdy, &pdz);
                let hit = HitPoint::new(min_t, normal, &ray);
                Some(hit)
            }
        }
    };

    Box::pin(hit_function)
}

/*
    Returns a hitfunction for larger than 3rd degree polynomials

    Input:
    - (i32, simpleIntExpr) vec (an optimized form of a polynomial)
    - derivative with respect to x
    - derivative with respect to y
    - derivative with respect to z

    Uses a sturm sequence chain to get an initial guess, which is then passed to the newtonRaphson function
    if the newtonRaphson result is outside the initial guess interval (where we know the smallest real root exists),

    we try again. This is stopped when a good approximation of the smallest root is found, no result has been found, or we have done the entire operation 5 times.
*/
fn hit_function_higher_degree(poly: Vec<(i32, IntSimpleExpr)>, pdx: Expr, pdy: Expr, pdz: Expr) -> HitFunction {
    let hit_function = move |ray: &Ray| {
        let mut lo = 0.0;
        let mut hi = 100.0;
        let mut max_depth = 15;
        let runs = 5;
        let iterations = 5;
        
        let ray_values = get_ray_values(&ray);
        // now that we now the ray values, we can turn our multivariable polynomial into a univariate one
        let up = uni_poly::to_uni_poly(&poly, ray_values);
        let derivative = uni_poly::uni_poly_derivative(&up);

        let sturm_seq = uni_poly::sturm_seq(&up, &derivative);
        for _ in 0..iterations {
            match uni_poly::get_interval(&sturm_seq, lo, hi, max_depth) {
                None            => return None,
                Some((ilo, ihi))  => {
                    lo = ilo;
                    hi = ihi;
                    let mid = lo + (hi - lo) / 2.0;
                    match newton_raphson(&up, &derivative, runs, mid) {
                        None    => return None,
                        Some(t) =>
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
                                let normal = normal_vector(&point_time, &pdx, &pdy, &pdz);
                                let hit = HitPoint::new(t, normal, &ray);
                                return Some(hit);
                            }
                    }
                }
            }
        }
        None
    };

    Box::pin(hit_function)
}

/*
    the juicy part!

    From a equation in string form:
    - parses it to an Expr.
    - creates partial derivates for x, y, and z.
    - substitutes the ray values into the Expr.
    - converts Expr to polynomial (as a HashMap).
    - converts the poly to a vec.
    - converts all SimpleExpr in the vec to IntSimpleExpr.
    
    Sets the hit function to a hit function of the correct degree, and with the poly.

    Returns a BaseShape, that given a Texture, can be converted to a Shape, with the to_shape() function.
    That Shape contains the hit function mentioned earlier, and an is_inside() function.
*/
pub fn make_implicit(s: String, material: Box<dyn Material>) -> BaseShape {
    // parsing the equation string to Expr
    let exp = expr::parse_string(s);
    // partial derivatives, needed for the nromal, returned when a hit occurs
    let pdx = partial_derivative(&exp, "x".into());
    let pdy = partial_derivative(&exp, "y".into());
    let pdz = partial_derivative(&exp, "z".into());
    // converting the Expr to a polynomial
    let poly = poly::expr_to_poly(subst_with_ray(&exp), "t".into());

    //println!("POLY: {:?}\n", poly);
    // turn the poly into a vec of (i32, IntSimpleExpr) tuples
    let poly_vec = uni_poly::to_int_simple_expr_vec(poly::poly_as_list(poly));
    //println!("POLY_VEC: {:?}\n", poly_vec);

    let hit_function: HitFunction = match poly_vec.last().expect("should not be empty").0 {
        1 => hit_function_first_degree(poly_vec, pdx, pdy, pdz),
        2 => hit_function_second_degree(poly_vec, pdx, pdy, pdz),
        _ => hit_function_higher_degree(poly_vec, pdx, pdy, pdz),
    };
    
    //println!("so far so good");

    let is_inside = Box::pin(
        move |p: &Point| expr::solve_expr(&exp, &p) > 0.0
    );

    BaseShape::new(is_inside, hit_function, material)
}