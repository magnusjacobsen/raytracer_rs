use std::collections::HashMap;
use std::hash::{Hash, Hasher};

use crate::bx;
use crate::implicit_surfaces::expr_parse::Expr;

/*
    One more type: atom
    ARadical gets removed before the function simplify is called
    -- not quite sure how Rusts handles these recursive types
*/
#[derive(Clone, Debug)]
pub enum Atom {
    Num(f32),
    Exponent(String, i32),
    Radical(Box<SimpleExpr>, i32)
}
type AtomGroup = Vec<Atom>;
type SimpleExpr = Vec<AtomGroup>;

impl Hash for Atom {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Num(f)            => state.write(&(*f as i32).to_ne_bytes()),
            Self::Exponent(s, i)    => {
                s.hash(state);
                i.hash(state);
            },
            Self::Radical(se, i)    => {
                i.hash(state);
                se.hash(state);
            }
        }
    }
}

impl PartialEq for Atom {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Num(f1) => {
                if let Self::Num(f2) = other {
                    f1 == f2
                } else {
                    false
                }
            },
            _ => self == other
        }
    }
}

impl Eq for Atom {}

/*
    Substitutes an FVar expression e, with another expression, ex, if the FVar's variable name matches x
*/
fn subst(e: Expr, x: &str, exp: &Expr) -> Expr {
    match e {
        Expr::Var(s)            => if s == x { (*exp).clone() } 
                                   else { Expr::Var(s.into()) },
        Expr::Add(a, b)         => Expr::Add(
                                    bx!(subst(*a, x, exp)),
                                    bx!(subst(*b, x, exp))
                                   ),
        Expr::Mul(a, b)         => Expr::Mul(
                                    bx!(subst(*a, x, exp)),
                                    bx!(subst(*b, x, exp))
                                   ),
        Expr::Exponent(a, i)    => Expr::Exponent(
                                    bx!(subst(*a, x, exp)),
                                    i
                                   ),
        Expr::Div(a, b)         => Expr::Div(
                                    bx!(subst(*a, x, exp)),
                                    bx!(subst(*b, x, exp))
                                   ),
        Expr::Root(a, i)        => Expr::Root(
                                    bx!(subst(*a, x, exp)),
                                    i
                                   ),
        _                       => e, // FNum
    }
}

fn is_SE_empty(se: SimpleExpr) -> bool {
    se.len() == 0 || se[0].len() == 0
}

/*
    Combines two SimpleExpr's
*/
fn combine_rec(xs: &SimpleExpr, ys: &[AtomGroup]) -> SimpleExpr {
    match ys.len() {
        0 => vec![],
        _ => { xs.iter().map(|x| x.iter().cloned().chain( ys[0].iter().cloned() ).collect::<Vec<_>>() ).chain(combine_rec(xs, &ys[1..]).iter().cloned()).collect::<Vec<_>>()
        }
    }
}

pub fn combine(xs: SimpleExpr, ys: SimpleExpr) -> SimpleExpr {
    combine_rec(&xs, &ys[..])
}

/*
    Converts an Expr to SimpleExpr, and as the type implies, simplifies the Expr in the process
*/
fn simplify(exp: Expr) -> SimpleExpr {
    match exp {
        Expr::Num(c)            => vec![vec![Atom::Num(c)]],
        Expr::Var(s)            => vec![vec![Atom::Exponent(s, 1)]],
        Expr::Exponent(_, 0)    => vec![vec![Atom::Num(1.0)]],
        Expr::Exponent(e1, 1)   => simplify(*e1),
        Expr::Exponent(e1, n)   => {
            if n < 0 {
                match *e1 {
                    Expr::Num(c)   => combine(simplify(Expr::Num(1.0 / c)), simplify(Expr::Exponent(e1, n + 1))),
                    Expr::Var(ref s)   => {
                        if n == -1 {
                            vec![vec![Atom::Exponent(s.clone(), -1)]]
                        } else {
                            combine(vec![vec![Atom::Exponent(s.clone(), -1)]], simplify(Expr::Exponent(e1, n + 1)))
                        }
                    },
                    _ => panic!("simplify: unmatched expr, shouldn't end here")
                }
            } else { // the exponent n is higher than 1
                combine(simplify(*e1.clone()), simplify(Expr::Exponent(e1, n - 1)))
            }
        },
        Expr::Root(e1, n)       => vec![vec![Atom::Radical(bx!(simplify(*e1)), n)]],
        Expr::Add(e1, e2)       => simplify(*e1).iter().chain(simplify(*e2).iter()).cloned().collect::<Vec<_>>(),
        Expr::Mul(e1, e2)       => combine(simplify(*e1), simplify(*e2)),
        Expr::Div(e1, e2)       => combine(simplify(*e1), simplify(*e2)),
    }
}

/*
    Returns the highest nth root of a SimpleExpr
*/
fn highest_root(se: SimpleExpr) -> i32 {
    let mut c = 0;
    let mut slice = &se[..];
    while slice.len() > 0 {
        c = slice[0].iter().fold(c, |cc, atom| {
            match atom {
                Atom::Num(_)        => cc,
                Atom::Exponent(..)  => cc,
                Atom::Radical(_, n) => cc.max(*n),
            }
        });
        slice = &slice[1..];
    }
    c
}

/*
    Check if a SimpleExpr contains a radical sign
*/
fn contains_root(se: &SimpleExpr) -> bool {
    for atomGroup in se {
        for atom in atomGroup {
            if let Atom::Radical(..) = atom {
                return true;
            }
        }
    }
    false
}

fn simplify_expr_add(e1: Expr, e2: Expr, changed: bool) -> (Expr, bool) {
    match (e1, e2) {
        // constants
        (Expr::Num(c1), Expr::Num(c2)) => (Expr::Num(c1 + c2), true),
        // case 9: e1 + (e2 / e3) = ((e1 * e3) + e2) / e3
        (e1, Expr::Div(e2, e3)) => {
            let (new_e1,_) = simplify_expr_rec(e1, true);
            let (new_e2,_) = simplify_expr_rec(*e2, true);
            let (new_e3,_) = simplify_expr_rec(*e3, true);
            (Expr::Div(
                bx!(Expr::Add(
                    bx!(Expr::Mul(bx!(new_e1), bx!(new_e3.clone()))),
                    bx!(new_e2)
                )),
                bx!(new_e3)
            ), true)
        },
        // case 9 (e2 / e3) + e1  = ((e1 * e3) + e2) / e3
        (Expr::Div(e2, e3), e1) => {
            let (new_e1,_) = simplify_expr_rec(e1, true);
            let (new_e2,_) = simplify_expr_rec(*e2, true);
            let (new_e3,_) = simplify_expr_rec(*e3, true);
            (Expr::Div(
                bx!(Expr::Add(
                    bx!(Expr::Mul(bx!(new_e1), bx!(new_e3.clone()))),
                    bx!(new_e2)
                )),
                bx!(new_e3)
            ), true)
        },
        // simple recursive situation
        (e1, e2) => {
            let (new_e1, new_changed) = simplify_expr_rec(e1, changed);
            let (new_e2, new_new_changed) = simplify_expr_rec(e2, new_changed);
            (Expr::Add(bx!(new_e1), bx!(new_e2)), new_new_changed)
        }
    }
}

fn simplify_expr_mul(e1: Expr, e2: Expr, changed: bool) -> (Expr, bool) {
    match (e1, e2) {
        // constants
        (Expr::Num(c1), Expr::Num(c2)) => (Expr::Num(c1 * c2), true),
        // case 6: e1 * (e2 / e3) = (e1 * e2) / e3
        (e1, Expr::Div(e2, e3)) => {
            let (new_e1,_) = simplify_expr_rec(e1, true);
            let (new_e2,_) = simplify_expr_rec(*e2, true);
            let (new_e3,_) = simplify_expr_rec(*e3, true);
            (Expr::Div(
                bx!(Expr::Mul(bx!(new_e1), bx!(new_e2))),
                bx!(new_e3)
            ), true)
        },
        // case 6: (e2 / e3) * e1 = (e1 * e2) / e3
        (Expr::Div(e2, e3), e1) => {
            let (new_e1,_) = simplify_expr_rec(e1, true);
            let (new_e2,_) = simplify_expr_rec(*e2, true);
            let (new_e3,_) = simplify_expr_rec(*e3, true);
            (Expr::Div(
                bx!(Expr::Mul(bx!(new_e1), bx!(new_e2))),
                bx!(new_e3)
            ), true)
        }
        // simple recursive situation
        (e1, e2) => {
            let (new_e1, new_changed) = simplify_expr_rec(e1, changed);
            let (new_e2, new_new_changed) = simplify_expr_rec(e2, new_changed);
            (Expr::Mul(bx!(new_e1), bx!(new_e2)), new_new_changed)
        }
    }
}

fn simplify_expr_div(e1: Expr, e2: Expr, changed: bool) -> (Expr, bool) {
    match (e1, e2) {
        // constants
        (Expr::Num(c1), Expr::Num(c2)) => (Expr::Num(c1 / c2), true),
        // case 10: (e1 / e2) / (e3 / e4) = (e1 * e4) / (e2 * e3)
        (Expr::Div(e1, e2), Expr::Div(e3, e4)) => {
            let (new_e1,_) = simplify_expr_rec(*e1, true);
            let (new_e2,_) = simplify_expr_rec(*e2, true);
            let (new_e3,_) = simplify_expr_rec(*e3, true);
            let (new_e4,_) = simplify_expr_rec(*e4, true);
            (Expr::Div(
                bx!(Expr::Mul(bx!(new_e1), bx!(new_e4))),
                bx!(Expr::Mul(bx!(new_e2), bx!(new_e3)))
            ), true)
        }
        // case 7: (e1 / e2) / e3 = e1 / (e2 * e3)
        (Expr::Div(e1, e2), e3) => {
            let (new_e1,_) = simplify_expr_rec(*e1, true);
            let (new_e2,_) = simplify_expr_rec(*e2, true);
            let (new_e3,_) = simplify_expr_rec(e3, true);
            (Expr::Div(
                bx!(new_e1),
                bx!(Expr::Mul(bx!(new_e2), bx!(new_e3)))
            ), true)
        },
        // case 8: e1 / (e2 / e3) = (e1 * e3) / e2
        (e1, Expr::Div(e2, e3)) => {
            let (new_e1,_) = simplify_expr_rec(e1, true);
            let (new_e2,_) = simplify_expr_rec(*e2, true);
            let (new_e3,_) = simplify_expr_rec(*e3, true);
            (Expr::Div(
                bx!(Expr::Mul(bx!(new_e1), bx!(new_e3))),
                bx!(new_e2)
            ), true)
        }
        // simple recursive situation
        (e1, e2) => {
            let (new_e1, new_changed) = simplify_expr_rec(e1, changed);
            let (new_e2, new_new_changed) = simplify_expr_rec(e2, new_changed);
            (Expr::Div(bx!(new_e1), bx!(new_e2)), new_new_changed)
        }
    }
}

fn simplify_expr_exp(e1: Expr, n: i32, changed: bool) -> (Expr, bool) {
    match (e1, n) {
        // constant
        (Expr::Num(c1), n)  => (Expr::Num(c1.powi(n)), true),
        (_, 0)              => (Expr::Num(1.0), true),
        (e1, 1)             => simplify_expr_rec(e1, true),
        // the simple recursive situation
        (e1, n)             => {
            let (new_e1, new_changed) = simplify_expr_rec(e1, changed);
            (Expr::Exponent(bx!(new_e1), n), new_changed)
        }
    }
}

fn simplify_expr_root(e1: Expr, n: i32, changed: bool) -> (Expr, bool) {
    match e1 {
        // constant
        Expr::Num(c1) => (Expr::Num(c1.powf(1.0 / n as f32)), true),
        // simple recursive situation
        e1 => {
            let (new_e1, new_changed) = simplify_expr_rec(e1, changed);
            (Expr::Exponent(bx!(new_e1), n), new_changed)
        }
    }
}

/*
    Simplifies an Expr by picking the low-hanging fruit
    Does not actually create a SimpleExpr, use simplify() for that
*/
fn simplify_expr_rec(exp: Expr, changed: bool) -> (Expr, bool) {
    match exp {
        Expr::Add(e1, e2)       => simplify_expr_add(*e1, *e2, changed),
        Expr::Mul(e1, e2)       => simplify_expr_mul(*e1, *e2, changed),
        Expr::Div(e1, e2)       => simplify_expr_div(*e1, *e2, changed),
        Expr::Exponent(e1, n)   => simplify_expr_exp(*e1, n, changed),
        Expr::Root(e1, n)       => simplify_expr_root(*e1, n, changed),
        _                       => (exp, changed), // Var and Num
    }
}

/* 
    Runs simplify_expr_rec until nothing changes
*/
fn simplify_expr(exp: Expr) -> Expr {
    let mut current = exp;
    loop {
        let (new_exp, changed) = simplify_expr_rec(current, false);
        if !changed {
            return new_exp;
        }
        current = new_exp;
    }    
}

/*
    Checks if nth radicals appear multiplied with itself n times
    if that is the case, the rooted term is all that is left (1 time)
    eg: x_3 * x_3 * x_3 = x
*/
fn remove_n_roots(se: SimpleExpr) -> SimpleExpr {
    fn inner(atomGroup: &AtomGroup) -> SimpleExpr {
        let mut roots = HashMap::new();
        let mut freed = vec![vec![]];
        let mut rest = vec![vec![]];
        for atom in atomGroup {
            match atom {
                Atom::Num(_)        => rest = combine(vec![vec![atom.clone()]], rest),
                Atom::Exponent(..)  => rest = combine(vec![vec![atom.clone()]], rest),
                Atom::Radical(x, n) => {
                    if let Some(v) = roots.get(&atom) {
                        if *n == (*v + 1) {
                            freed = combine((**x).clone(), freed);
                            roots.remove(&atom);
                        } else {
                            roots.insert(atom, v + 1);
                        }
                    } else {
                        roots.insert(atom, 1);
                    }
                }
            }
        }
        for (atom, count) in roots {
            let remaining = (0..count).fold(vec![vec![]], |acc, _| combine(acc, vec![vec![atom.clone()]]));
            rest = combine(rest, remaining);
        }
        combine(rest, freed)
    }
    se.iter().fold(vec![], |acc, ag| acc.iter().chain(inner(ag).iter()).cloned().collect::<Vec<_>>())
}