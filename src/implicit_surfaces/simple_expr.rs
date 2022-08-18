use rustc_hash::FxHashMap;
use std::hash::{Hash, Hasher};

use crate::bx;
use crate::implicit_surfaces::expr::Expr;

/*
    Atom is an element in a term, eg in (x * 1), both x and 1 are terms
    - Num is simply a constant (of float)
    - Exponent is a variable, where for instance x^1 is explicit instead of explicit
    - Root/Radical is for terms to be rooted,
    --> all roots gets removed before the function simplify is called
*/
#[derive(Clone, Debug)]
pub enum Atom {
    Num(f32),
    Exponent(String, i32),
    Root(Box<SimpleExpr>, i32)
}
/*
    AtomGroup is a term, where for each Atom in the Vec, there exists an implicit multiplication between them
*/
pub type AtomGroup = Vec<Atom>;
/*
    SimpleExpr is the one side expression of an equation, where between each AtomGroups there exists an implicit addition

    An example: (y * x^2) + (x * y^3)
    where each paranthesis-group contains an AtomGroup of Atoms
*/
pub type SimpleExpr = Vec<AtomGroup>;

impl Hash for Atom {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Num(f)            => state.write(&(*f as i32).to_ne_bytes()),
            Self::Exponent(s, i)    => {
                s.hash(state);
                i.hash(state);
            },
            Self::Root(se, i)    => {
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
            Self::Exponent(s, i) => {
                if let Self::Exponent(o_s, o_i) = other {
                    s == o_s && i == o_i
                } else {
                    false
                }
            },
            Self::Root(se, i) => {
                if let Self::Root(o_se, o_i) = other {
                    se == o_se &&  i == o_i
                } else {
                    false
                }
            }
        }
    }
}

impl Eq for Atom {}

fn se_empty() -> SimpleExpr {
    vec![vec![]]
}

fn atom_to_se(atom: Atom) -> SimpleExpr {
    vec![vec![atom]]
}

pub fn se_num(c: f32) -> SimpleExpr {
    atom_to_se(Atom::Num(c))
}

fn se_exp(s: String, n: i32) -> SimpleExpr {
    atom_to_se(Atom::Exponent(s, n))
}

fn se_root(se: Box<SimpleExpr>, n: i32) -> SimpleExpr {
    atom_to_se(Atom::Root(se, n))
}

/*
    Substitutes an FVar expression e, with another expression, ex, if the FVar's variable name matches x
*/
fn _subst(e: Expr, x: &str, exp: &Expr) -> Expr {
    match e {
        Expr::Var(s)            => if s == x { (*exp).clone() } 
                                   else { Expr::Var(s.into()) },
        Expr::Add(a, b)         => Expr::Add(
                                    bx!(_subst(*a, x, exp)),
                                    bx!(_subst(*b, x, exp))
                                   ),
        Expr::Mul(a, b)         => Expr::Mul(
                                    bx!(_subst(*a, x, exp)),
                                    bx!(_subst(*b, x, exp))
                                   ),
        Expr::Exponent(a, i)    => Expr::Exponent(
                                    bx!(_subst(*a, x, exp)),
                                    i
                                   ),
        Expr::Div(a, b)         => Expr::Div(
                                    bx!(_subst(*a, x, exp)),
                                    bx!(_subst(*b, x, exp))
                                   ),
        Expr::Root(a, i)        => Expr::Root(
                                    bx!(_subst(*a, x, exp)),
                                    i
                                   ),
        _                       => e, // FNum
    }
}

fn is_empty(se: &SimpleExpr) -> bool {
    se.len() == 0 || se[0].len() == 0
}

/*
    Combines two SimpleExprs
    - equivalent to multiplying an expression in an equation with another.
    - example: combine(xs = x + z, ys = y + 3)
        xs = x * (y + 3) + z * (y + 3)
        xs = (x * y) + (x * 3) + (z * y) + (z * 3)
*/
pub fn combine(xs: &SimpleExpr, ys: &SimpleExpr) -> SimpleExpr {
    if is_empty(&ys) {
        xs.clone()
    } else {
        xs
            .iter()
            .map(|x| {
                ys
                    .iter()
                    .cloned()
                    .map(|y| x
                        .iter()
                        .cloned()
                        .chain( y.iter().cloned() )
                        .collect::<Vec<_>>()
                    ).collect::<Vec<_>>()
            })
            .flatten()
            .collect::<Vec<_>>()

    }       
}

/*
    Converts an Expr to SimpleExpr, and as the type implies, simplifies the Expr in the process
*/
pub fn simplify(exp: Expr) -> SimpleExpr {
    match exp {
        Expr::Num(c)            => se_num(c),
        Expr::Var(s)            => se_exp(s, 1),
        Expr::Exponent(_, 0)    => se_num(1.0),
        Expr::Exponent(e1, 1)   => simplify(*e1),
        Expr::Exponent(e1, n)   => {
            if n < 0 {
                match *e1 {
                    Expr::Num(c)   => combine(&simplify(Expr::Num(1.0 / c)), &simplify(Expr::Exponent(e1, n + 1))),
                    Expr::Var(ref s)   => {
                        if n == -1 {
                            se_exp(s.clone(), -1)
                        } else {
                            combine(&se_exp(s.clone(), -1), &simplify(Expr::Exponent(e1, n + 1)))
                        }
                    },
                    _ => panic!("simplify: unmatched expr, shouldn't end here")
                }
            } else { // the exponent n is higher than 1
                combine(&simplify(*e1.clone()), &simplify(Expr::Exponent(e1, n - 1)))
            }
        },
        Expr::Root(e1, n)       => se_root(bx!(simplify(*e1)), n),
        Expr::Add(e1, e2)       => simplify(*e1).iter().chain(simplify(*e2).iter()).cloned().collect::<Vec<_>>(),
        Expr::Mul(e1, e2)       => combine(&simplify(*e1), &simplify(*e2)),
        // e1 / e2 is the same as e1 * e2^-1 (because e2^-1 = 1 / e2)
        Expr::Div(e1, e2)       => combine(&simplify(*e1), &simplify(Expr::Exponent(bx!(*e2), -1))),
    }
}

/*
    Returns the highest nth root of a SimpleExpr
*/
fn highest_root(se: &SimpleExpr) -> i32 {
    se.iter().fold(0, |k, ag| {
        ag.iter().fold(k, |ag_k, atom|
            if let Atom::Root(_, n) = atom { ag_k.max(*n) } 
            else { ag_k }
        )
    })
}

fn contains_roots_ag(ag: &AtomGroup) -> bool {
    for atom in ag {
        if let Atom::Root(..) = atom {
            return true;
        }
    }
    false
}

/*
    Check if a SimpleExpr contains a Root sign
*/
fn contains_roots_se(se: &SimpleExpr) -> bool {
    for atom_group in se {
        for atom in atom_group {
            if let Atom::Root(..) = atom {
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
    Checks if nth Roots appear multiplied with itself n times
    if that is the case, the rooted term is all that is left (1 time)
    eg: x_3 * x_3 * x_3 = x
    TODO: could this be done simpler?
*/
fn remove_n_roots(se: SimpleExpr) -> SimpleExpr {
    fn inner(atom_group: &AtomGroup) -> SimpleExpr {
        let mut roots = FxHashMap::default();
        let mut freed = se_empty();
        let mut rest = se_empty();
        for atom in atom_group {
            match atom {
                Atom::Num(_)        => rest = combine(&rest, &atom_to_se(atom.clone())),
                Atom::Exponent(..)  => rest = combine(&vec![vec![atom.clone()]], &rest),
                Atom::Root(x, n) => {
                    if let Some(v) = roots.get(&atom) {
                        if *n == (*v + 1) {
                            freed = combine(&(**x), &freed);
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
            let remaining = (0..count).fold(se_empty(), |acc, _| combine(&acc, &atom_to_se(atom.clone())));
            rest = combine(&rest, &remaining);
        }
        combine(&rest, &freed)
    }
    se.iter().fold(vec![], |acc, ag| acc.iter().chain(inner(ag).iter()).cloned().collect::<Vec<_>>())
}

/*
    Takes a SimpleExpr, and first checks if there are any roots present
    If that is the case:
        - Finds the highest root, k, inverts all the terms without roots, which is equivaent to moving these terms to the other side of the equation sign.
        - Multiplies both sides of the equation with itself, k times, and inverts the terms without roots once again (to "move them back to the other side of the equation sign").
        - Call remove_n_roots on the collection with roots, and then merges the result with the non-root terms

        If the combined result still contains_roots(), simplify_roots() is called once again. Otherwise the result is simply returned.
*/
fn simplify_roots(se: SimpleExpr) -> SimpleExpr {
    if contains_roots_se(&se) {
        let se_removed = remove_n_roots(se);
        // split se_removed up in one SimpleExpr with roots, and one without
        let mut no_roots = vec![];
        let mut roots = vec![];
        se_removed.iter().for_each( |ag|
            if contains_roots_ag(ag) {
                roots.push(ag.clone());
            } else {
                no_roots.push(ag.clone());
            }
        );
        if !roots.is_empty() {
            let k = highest_root(&roots);

            // no_roots multiply by -1, then to the power of k, and then again multiply by -1
            let no_roots_invert = combine(&no_roots, &se_num(-1.0));
            let no_roots_multiplied = (0..k).fold(se_empty(), |acc, _| combine(&acc, &no_roots_invert));
            let no_roots_done = combine(&no_roots_multiplied, &se_num(-1.0));

            // multiply the roots terms, but don't invert
            let roots_multiplied = (0..k).fold(se_empty(), |acc, _| combine(&acc, &roots));
            let mut roots_done = remove_n_roots(roots_multiplied);

            // put no_roots terms back with roots terms
            roots_done.extend(no_roots_done);

            if contains_roots_se(&roots_done) {
                simplify_roots(roots_done)
            } else {
                roots_done
            }
        } else {
            no_roots
        }
    } else {
        se
    }
}

/*
    Takes an AtomGroup (equivalent to a term in an equation)
    Puts similar parts of the term together, and counts how many times they appear (their exponents)
    Also, sums constants together into a single one

    NB: at this point there should be no roots, so we only consider Nums and Exponents!
*/
pub fn simplify_atom_group(ag: &AtomGroup) -> AtomGroup {
    let mut constant = 1.0;
    let mut exps = FxHashMap::default();
    for a in ag {
        match a {
            Atom::Num(n)            => constant *= n,
            Atom::Exponent(s, n)    => {
                // x^0 would be 1, and should this be ignored
                if *n != 0 {
                    *exps.entry(s).or_insert(0) += n;
                }
            },
            _ => panic!("simplify_atom_group: There should not be any roots at this point!"),
        }
    };
    let mut exps_vec = exps.into_iter().map(|(k, v)| 
         Atom::Exponent(k.to_string(), v)).collect::<Vec<_>>();

    if constant == 0.0 {
        // a whatever the rest it, multiplying by 0 is 0
        vec![]
    } else {
        exps_vec.insert(0, Atom::Num(constant));
        exps_vec
    }
}

/*
    Counts how many times equal terms appear, reduces them to a single term multiplied by the count.
    All terms of only constants are summed to a single term.
*/
pub fn simplify_simple_expr(se: SimpleExpr) -> SimpleExpr {
    let simplified = se.iter().map(|ag| simplify_atom_group(ag)).collect::<Vec<_>>();

    let mut constant = 0.0;
    let mut vars = FxHashMap::default();

    for ag in &simplified {
        match ag.len() {
            0   => (),
            len =>
                match ag[0] {
                    Atom::Num(c) => {
                        if len == 1 {
                            constant += c
                        } else {
                            *vars.entry(&ag[1..]).or_insert(0.0) += c;
                        }
                    },
                    _ => panic!("simplify_simple_expr: first item should always be a constant at this point in execution"),
                },
        }
    };

    // last task is to group similar AtomGroups into one group
    let mut vars_vec = vars.iter()
        .map(|(k,v)| {
            if *v == 1.0 {
                k.to_vec()
            } else {
                let mut ag = vec![Atom::Num(*v)];
                ag.extend_from_slice(k);
                ag
            }
        })
        .collect::<Vec<_>>();
    if constant != 0.0 {
        vars_vec.push(vec![Atom::Num(constant)]);
    }
    vars_vec
}

/*
    Invoking the spirit of Muhammad ibn Musa al-Khwarizmi
*/
fn rewrite_expr(exp: Expr) -> SimpleExpr {
    let reduced = match simplify_expr(exp) {
        Expr::Div(keep, _)  => *keep,
        res                 => res,
    };

    simplify_roots(simplify(reduced))
}

/*
    Runs most of the code from this file
    It takes an Expr as input and returns a SimpleExpr

    This simpleExpr contains no divisions or radical/root signs
*/
pub fn expr_to_simple_expr(exp: Expr) -> SimpleExpr {
    simplify_simple_expr(rewrite_expr(exp))
}