use super::simple_expr::{SimpleExpr, Atom};

// Very small number. Considered as good as zero
// 10^-20
const EPSILON: f64 = 0.00000000000000000001;

/*
    Similar to SimpleExpr, AtomgGroup and Atom
    but with the difference that exponents contain an integer instead of a string variable

    The integers are always mapped as follows:
        ox -> 0
        oy -> 1
        oz -> 2
        dx -> 3
        dy -> 4
        dz -> 5
*/
#[derive(Clone, Debug)]
pub enum IntAtom {
    Num(f64),
    Exponent(usize, i32),
}

pub type IntAtomGroup = Vec<IntAtom>;

pub type IntSimpleExpr = Vec<IntAtomGroup>;

/*
    UniPoly represents an univariate polynomial, ie. only one variable, t, which is implicitly present in all map elements of a Poly
*/
pub type UniPoly = Vec<(i32, f64)>;

/*
    Turns a SimpleExpr into an IntSimpleExpr
*/
pub fn to_int_simple_expr(se: &SimpleExpr) -> IntSimpleExpr {
    se
        .into_iter()
        .map(|ag|
            ag
                .into_iter()
                .map(|atom|
                    match atom {
                        Atom::Num(c) => IntAtom::Num(*c),
                        Atom::Exponent(e,x) =>
                            match e.as_str() {
                                "ox" => IntAtom::Exponent(0, *x),
                                "oy" => IntAtom::Exponent(1, *x),
                                "oz" => IntAtom::Exponent(2, *x),
                                "dx" => IntAtom::Exponent(3, *x),
                                "dy" => IntAtom::Exponent(4, *x),
                                "dz" => IntAtom::Exponent(5, *x),
                                _    => panic!("to_int_simple_expr: unmatched Atom::Exponent variable clause"),
                            },
                        _ => panic!("to_int_simple_expr: unmatched Atom::Root clause"),
                    }
                )
                .collect::<Vec<_>>()
        )
        .collect::<Vec<_>>()    
}

/*
    Converts a (i32, SimpleExpr) vec into a (i32, IntSimpleExpr) vec
*/
pub fn to_int_simple_expr_vec(se_vec: Vec<(i32, SimpleExpr)>) -> Vec<(i32, IntSimpleExpr)> {
    se_vec
        .iter()
        .map(|(i, se)| (*i, to_int_simple_expr(se))
        )
        .collect()
}

/*
    Solves a IntSimpleExpr, with an array of ray values, where index positions match the first int of an IntAtom::Exponent

    Returns ?
*/
pub fn solve_int_simple_expr(ise: &IntSimpleExpr, ray_values: [f64; 6]) -> f64 {
    ise
        .iter()
        .map(|ag|
            ag
                .iter()
                .map(|atom|
                    match atom {
                        IntAtom::Num(c)         => *c,
                        IntAtom::Exponent(e, x) => ray_values[*e].powi(*x),
                    }
                )
                .product::<f64>()
        )
        .sum()
}

/*
    Takes a Poly Vec and converts it to an UniPoly ()

    Requires an array with all the ray float values (based on origin point and direction vector)

    TODO:
    quite confused here.
    comment says poly vec, but the function then takes a list of (Int, IntSimpleExpr),
    whereas a Poly is HashMap<i32, SimpleExpr>...
    I also don't know when dx, dy, dz... comes into the picture...

    also, when is this used???
    */
pub fn to_uni_poly(vec: &Vec<(i32, IntSimpleExpr)>, ray_values: [f64; 6]) -> UniPoly {
    vec
        .iter()
        .map(|(n, ise)|
            (*n, solve_int_simple_expr(ise, ray_values))
        )
        .rev()
        .collect()
}

/*
    Solves an UniPoly for a given t value
*/
pub fn solve_uni_poly(up: &UniPoly, t: f64) -> f64 {
    up
        .iter()
        .map(|(n, c)|
            if *n > 0 { t.powi(*n) * c }
            else { *c }
        )
        .sum()
}

/*
    Returns the derivative UniPoly of the given UniPoly
*/
pub fn uni_poly_derivative(up: &UniPoly) -> UniPoly {
    up
        .iter()
        .fold(vec![], |mut acc, (n, c)|
            if *n == 0 {
                acc
            } else {
                acc.push((*n - 1, *n as f64 * *c));
                acc
            }
        )
}

/*
    Multiplies an UniPoly with a term (of a constant and an exponent)
*/
fn multiply(up: &UniPoly, (exp, con): (i32, f64)) -> UniPoly {
    up
        .iter()
        .map(|(n, c)|
            (n + exp, c * con)
        )
        .collect()
}

/*
    Negates an UniPoly
*/
fn negate(up: &UniPoly) -> UniPoly {
    up
        .iter()
        .map(|(n, c)| (*n, -c))
        .collect()
}

/*
    Subtracts one UniPoly (up2) from another (up1)
*/
fn subtract(up1: &UniPoly, up2: &UniPoly) -> UniPoly {
    let len1 = up1.len();
    let len2 = up2.len();
    let mut idx1 = 0;
    let mut idx2 = 0;
    let mut out = vec![];

    while idx1 < len1 || idx2 < len2 {
        if idx1 == len1 {
            out.extend(&up2[idx2..]);
            idx2 = len2;
        } else {
            let (n1, c1) = up1[idx1];
            if idx2 == len2 {
                if c1.abs() >= EPSILON {
                    out.push((n1, c1));
                }
                idx1 += 1;
            } else {
                let (n2, c2) = up2[idx2];
                if n1 == n2 {
                    let v = c1 - c2;
                    if v.abs() >= EPSILON {
                        out.push((n1, v));
                    }
                    idx1 += 1;
                    idx2 += 1;
                } else if n2 < n1 {
                    out.push((n1, c1));
                    idx1 += 1;
                } else {
                    out.push((n2, -c2));
                    idx2 += 1;
                }
            }
        }
    }
    out
}

/*
    Polynomial Long Division on two UniPolys, f and s.
    f is the dividend, s is the divisor.

    It is assumed that up is of a lower degree than up1.

    Only the remainder of the operation is returned,
    ie. the quotient is not collected.

    Logis is inspired from https://rosettacode.org/wiki/Polynomial_long_division#OCaml

    Potential for an endless loop, if no epsilon is used in subtract(), or if the epsilon is too big.
*/
fn long_division_remainder(f: &UniPoly, s: &UniPoly) -> UniPoly {
    // the difference between the degree of f and s is negative
    if f[0].0 - s[0].0 < 0 {
        f.clone()
    } else {
        let (f_exp, f_const) = f[0];
        let (s_exp, s_const) = s[0];
        // division of the two terms
        let k = (f_exp - s_exp, f_const / s_const);
        let ks = multiply(&s, k);
        let new_f = subtract(&f, &ks);
        if new_f.is_empty() {
            f.clone()
        } else {
            long_division_remainder(&new_f, s)
        }
    }
}

/*
    Generates a Sturm sequence chain for an UniPoly and its derivate
*/
pub fn sturm_seq(up: &UniPoly, derivative: &UniPoly) -> Vec<UniPoly> {
    let mut out = vec![up.clone(), derivative.clone()];
    let mut last = 1;
    let mut count = 0;
    while out[last][0].0 > 0 && count < 1000 {
        count += 1;
        let new_up = long_division_remainder(&out[last - 1], &out[last]);
        out.push(negate(&new_up));
        last += 1;
    }
    out
}

/*
    Counts sign changes in a UniPoly Vec, for a given value inserted in the variable's place
*/
fn count_sign_changes(up_vec: &Vec<UniPoly>, t: f64) -> i32 {
    let mut prev = solve_uni_poly(&up_vec[0], t);
    let mut count = 0;
    for i in 1..up_vec.len() {
        let current = solve_uni_poly(&up_vec[i], t);
        if current.signum() != prev.signum() {
            count += 1;
        }
        prev = current;
    }
    count
}

/*
    Finds the smallest interval where the smallest root lives.

    Uses binary search to recursively split the search space in halves, and checks the number of real roots that lives in the current space.

    Runs until depth reaches 0.
*/
pub fn get_interval(up_vec: &Vec<UniPoly>, lo: f64, hi: f64, depth: i32) -> Option<(f64, f64)> {
    let mut lo = lo;
    let mut hi = hi;
    let mut signs_lo = count_sign_changes(&up_vec, lo);
    let mut signs_hi = count_sign_changes(&up_vec, hi);
    let mut depth = depth;
    while depth > 0 {
        let mid = lo + (hi - lo) / 2.0;
        let signs_mid = count_sign_changes(&up_vec, mid);
        if signs_lo - signs_mid > 0 {
            hi = mid;
            signs_hi = signs_mid;
            depth -= 1;
            continue;
        } else if signs_mid - signs_hi > 0 {
            lo = mid;
            signs_lo = signs_mid;
            depth -= 1;
            continue;
        }
        return None;
    }
    Some((lo, hi))
}