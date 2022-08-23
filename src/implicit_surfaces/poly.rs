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

fn simple_expr_to_poly(se: SimpleExpr, v: &str) -> Poly {
    se.into_iter().fold(FxHashMap::default(), |poly, ag| 
        add_atom_group(v, poly, ag)
    )
}

/*
    Converts an Expr into a Poly (polynomial) with respect to a variable string V
*/
pub fn expr_to_poly(expr: Expr, v: &str) -> Poly {
    simple_expr_to_poly(simplify_simple_expr(expr_to_simple_expr(expr)), v)
}

/*
    Returns the derivate of a Poly, with respect to t
*/
pub fn poly_derivative(poly: Poly) -> Poly {
    let mut derivative = FxHashMap::default();
    for (order, se) in poly.iter() {
        if *order > 0 {
            let multiplied = combine(se, &se_num(*order as f64));
            let simplified = simplify_simple_expr(multiplied);
            derivative.insert(order - 1, simplified);
        }
    }
    derivative
}

/*
    Turns a Poly into a vec sorted by order (i32)
*/
pub fn poly_as_list(poly: &Poly) -> Vec<(i32, SimpleExpr)> {
    let mut vec = poly.clone().into_iter().collect::<Vec<_>>();
    vec.sort_by(|a,b| a.0.partial_cmp(&b.0).unwrap());
    vec
}

fn to_string_atom(atom: &Atom) -> String {
    match atom {
        Atom::Num(c)                    => format!("{c}"),
        Atom::Exponent(v, e) if *e == 1 => format!("{v}"),
        Atom::Exponent(v, e)            => format!("{v}^{e}"),
        _                               => format!(""),
    }
}

fn to_string_simple_expr(simple_expr: &SimpleExpr) -> String {
    simple_expr
        .iter()
        .map(|atom_group|
            atom_group
                .iter()
                .map(|atom| to_string_atom(atom))
                .collect::<Vec<_>>()
                .join("*")
        )
        .collect::<Vec<_>>()
        .join("+")
}

pub fn to_string_poly(v: &str, poly: &Poly) -> String {
    let poly_vec = poly_as_list(poly);
    poly_vec
        .iter()
        .map(|(d, simple_expr)| {
            let prefix = if *d > 0 {
                to_string_atom(&Atom::Exponent(v.to_string(), *d))
            } else { format!("") };
    
            let content = if simple_expr.is_empty() || simple_expr[0].is_empty() {
                format!("")
            } else {
                let se_string = to_string_simple_expr(simple_expr);
                format!("({se_string})")
            };
            format!("{prefix}{content}")
        })
        .collect::<Vec<_>>()
        .join("+")
}