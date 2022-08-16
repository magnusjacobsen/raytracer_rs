use rustc_hash::FxHashMap;

use super::simple_expr::{Atom, AtomGroup, SimpleExpr};

/* 
    our type alias Poly, short for polynomial
    the key is represents the order of the SimpleExpr (I think)
*/ 
type Poly = FxHashMap<i32, SimpleExpr>;

/*
    Collects AtomGroups (terms) into groups with respect to one variable v
    
    equivalent to taking the derivate with respect to a variable
*/
fn split_atom_group(v: &str, mut poly: Poly, ag: AtomGroup) -> Poly {
    if !ag.is_empty() {
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
                Atom::Exponent(..) => continue,
                _ => panic!("split_atom_group: should never meet an ANum here!!"),
            }
        }
        poly.entry(0).or_insert(vec![]).push(ag);
    }
    poly
} 

fn simple_expr_to_poly(se: SimpleExpr, v: String) -> Poly {
    se.into_iter().fold(FxHashMap::default(), |poly, ag| 
        split_atom_group(&v, poly, ag)
    )
} 