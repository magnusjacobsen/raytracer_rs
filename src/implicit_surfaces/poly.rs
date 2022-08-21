use rustc_hash::FxHashMap;

use super::simple_expr::{Atom, AtomGroup, SimpleExpr, simplify_simple_expr, expr_to_simple_expr, combine, se_num};
use super::expr::Expr;

/* 
    our type alias Poly, short for polynomial
    the key is represents the order of the SimpleExpr (I think)
*/ 
pub type Poly = FxHashMap<i32, SimpleExpr>;

/*
    Collects AtomGroups (terms) into groups with respect to one variable v
    
    equivalent to taking the derivate with respect to a variable
*/
fn add_atom_group(v: &str, mut poly: Poly, ag: AtomGroup) -> Poly {
    if ag.is_empty() {
        return poly;
    } else {
        for atom in &ag {
            match atom {
                Atom::Exponent(var, d) if var == &v => {
                    let splitted = ag
                        .iter()
                        .filter(|x| 
                            if let Atom::Exponent(var,_) = x {
                                var != v
                            } else { true }
                        )
                        .cloned()
                        .collect::<Vec<_>>();
                    poly.entry(*d).or_insert(vec![]).push(splitted);
                    return poly;
                },
                _ => continue,
            }
        }
        // if we never reached an exponent with variable v
        poly.entry(0).or_insert(vec![]).push(ag);
    }
    poly
}

fn simple_expr_to_poly(se: SimpleExpr, v: String) -> Poly {
    se.into_iter().fold(FxHashMap::default(), |poly, ag| 
        add_atom_group(&v, poly, ag)
    )
}

/*
    Converts an Expr into a Poly (polynomial) with respect to a variable string V
*/
pub fn expr_to_poly(expr: Expr, v: String) -> Poly {
    simple_expr_to_poly(simplify_simple_expr(expr_to_simple_expr(expr)), v)
}

/*
    Returns the derivate of a Poly, with respect to t
*/
pub fn poly_derivative(poly: Poly) -> Poly {
    let mut derivative = FxHashMap::default();
    for (order, se) in poly.iter() {
        if *order > 0 {
            let multiplied = combine(se, &se_num(*order as f32));
            let simplified = simplify_simple_expr(multiplied);
            derivative.insert(order - 1, simplified);
        }
    }
    derivative
}

/*
    Turns a Poly into a vec sorted by order (i32)
*/
pub fn poly_as_list(poly: Poly) -> Vec<(i32, SimpleExpr)> {
    let mut vec = poly.into_iter().collect::<Vec<_>>();
    vec.sort_by(|a,b| a.0.partial_cmp(&b.0).unwrap());
    vec
}
