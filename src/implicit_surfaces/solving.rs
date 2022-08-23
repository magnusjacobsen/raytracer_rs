use crate::bx;
use crate::core::point::Point;
use crate::core::ray::Ray;
use crate::materials::Material;
use crate::shapes::base_shape::BaseShape;
use crate::core::vector::Vector;
use super::expr::{self, Expr};
use super::hit_function::HitFunction;
use super::poly::{self};
use super::uni_poly::{self, UniPoly};

// const values used in the newton_raphson function
const TOLERANCE: f64 = 0.000001; // 10^-5
const EPSILON: f64 = 0.00000000001; // 10^-10

/*
    Substitutes ray variables (p + t * d) into an Expr, that represents an implicit surface
*/
pub fn subst_with_ray(exp: &Expr) -> Expr {
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
                        bx!(Expr::Num(*n as f64)),
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
                        bx!(Expr::Num(*n as f64)),
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
pub fn normal_vector(p: &Point, dx: &Expr, dy: &Expr, dz: &Expr) -> Vector {
    let x = expr::solve_expr(dx, p);
    let y = expr::solve_expr(dy, p);
    let z = expr::solve_expr(dz, p);
    Vector::new(x, y, z).normalize()
}

/*
    Calculates a discriminant from the quadratic equation values of a, b, and c.
*/
pub fn discriminant(a: f64, b: f64, c: f64) -> f64 {
    b.powi(2) - 4.0 * a * c
}

/*
    Returns two t-values in a vec, when given a quadratic equation's a, b, and discrimant.

    Requires that the discrimant is not negative (in that case there is no real solution)
*/
pub fn get_distances(a: f64, b: f64, dis: f64) -> Vec<f64> {
    let sres = dis.sqrt();
    let res_pos = (-b + sres) / (2.0 * a);
    let res_neg = (-b - sres) / (2.0 * a);
    vec![res_pos, res_neg]
}

/*
    Create a f64 array with values from the ray
*/
pub fn get_ray_values(ray: &Ray) -> [f64; 6] {
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
      but changed over time.
*/
pub fn newton_raphson(f: &UniPoly, df: &UniPoly, runs: usize, guess: f64) -> Option<f64> {
    let mut y = uni_poly::solve_uni_poly(&f, guess);
    let mut dy = uni_poly::solve_uni_poly(&df, guess);
    let mut guess = guess;
    let mut runs = runs;
    while runs > 0 {
        if dy.abs() < EPSILON {
            break;
        } else {
            let new_guess = guess - (y / dy);
            if (new_guess - guess).abs() <= (TOLERANCE * new_guess.abs()) {
                return Some(new_guess);
            } else {
                guess = new_guess;
                y = uni_poly::solve_uni_poly(&f, guess);
                dy = uni_poly::solve_uni_poly(&df, guess);
                runs -= 1;
            }
        }
    }
    None
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

    // turn the poly into a vec of (i32, IntSimpleExpr) tuples
    let poly_vec = uni_poly::to_int_simple_expr_vec(poly::poly_as_list(&poly));

    let hit_function: HitFunction = HitFunction::new(poly_vec, pdx, pdy, pdz);

    BaseShape::new(hit_function, material)
}