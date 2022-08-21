/*
    This module is able to scan and parse an equation string,
    and turn it into a expr format.
*/
use crate::{bx, core::point::Point};

// omg we have discriminated unions, like in F# :O
#[derive(PartialEq, Clone, Debug)]
pub enum Terminal {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Root,
    Lpar,
    Rpar,
    Int(i32),
    Float(f32),
    Var(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Num(f32),
    Var(String),
    Add(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Exponent(Box<Expr>, i32),
    Root(Box<Expr>, i32),
}

// helper functions
fn is_blank(c: char) -> bool {
    c.is_whitespace()
}

fn is_digit(c: char) -> bool {
    c.is_digit(10)
}

fn is_letter(c: char) -> bool {
    c.is_alphabetic()
}

fn is_letter_digit(c: char) -> bool {
    c.is_alphanumeric()
}

fn int_val(c: char) -> i32 {
    c.to_digit(10).expect("Could not transform char to int") as i32
}

fn float_val(c: char) -> f32 {
    int_val(c) as f32
}

/*************
    All the various scanner sub functions
***************/

fn apply_negates(inp: &[Terminal], mut out: Vec<Terminal>) -> Vec<Terminal> {
    match inp.len() {
        0 => return out,
        1 => {
            out.push(inp[0].clone());
            return out
        },
        _ => { // check if we have a Subtraction Terminal
            match &inp[0] {
                Terminal::Sub => {
                    // Directly invert if the Term following is a number, otherwise multiply by -1.0
                    match inp[1] {
                        Terminal::Float(f) => {
                            out.push(Terminal::Float(-f));
                            apply_negates(&inp[2..], out)
                        },
                        Terminal::Int(i) => {
                            out.push(Terminal::Int(-i));
                            apply_negates(&inp[2..], out)
                        },
                        _ => {
                            out.push(Terminal::Float(-1.0));
                            out.push(Terminal::Mul);
                            apply_negates(&inp[1..], out)
                        }
                    }
                }
                // Cases where there might be an implicit addition-subtraction
                Terminal::Rpar | Terminal::Int(_) | Terminal::Float(_) | Terminal::Var(_) => {
                    // add the implicit addition, and then invert the following Terminal with the next recursive call
                    if let Terminal::Sub = inp[1] {
                        out.push(inp[0].clone());
                        out.push(Terminal::Add);
                        apply_negates(&inp[1..], out)
                    } else {
                        out.push(inp[0].clone());
                        apply_negates(&inp[1..], out)
                    }
                },
                // all other cases
                x => {
                    out.push(x.clone());
                    apply_negates(&inp[1..], out)
                },
            }
        }
    }
}

/*
    Scans for a Float
*/
fn scan_frac(cs: &[char], value: f32, wt: f32) -> (&[char], Terminal) {
    if cs.len() > 0 && is_digit(cs[0]) {
        scan_frac(&cs[1..], value + wt * float_val(cs[0]), wt / 10.0)
    } else {
        (cs, Terminal::Float(value))
    }
}

/*
    Scans a number, returns the rest of the string and a Terminal of either Float or Int
*/
fn scan_num(cs: &[char], value: i32) -> (&[char], Terminal) {
    if cs.len() > 1 && cs[0] == '.' && is_digit(cs[1]) {
        scan_frac(&cs[1..], value as f32, 0.1)
    } else if cs.len() > 0 && is_digit(cs[0]) {
        scan_num(&cs[1..], 10 * value + int_val(cs[0]))
    } else {
        (cs, Terminal::Int(value))
    }
}

fn scan_name(cs: &[char], value: String) -> (&[char], String) {
    if cs.len() > 0 && is_letter_digit(cs[0]) {
        scan_name(&cs[1..], value + &cs[0].to_string())
    } else {
        (cs, value)
    }
}

fn scan_rec(cs: &[char], mut result: Vec<Terminal>) -> Vec<Terminal> {
    if cs.len() == 0 {
        return apply_negates(&result[..], vec![]);
    }

    match cs[0] {
        '+' => {
            result.push(Terminal::Add);
            scan_rec(&cs[1..], result)
        },
        '*' => {
            result.push(Terminal::Mul);
            scan_rec(&cs[1..], result)
        },
        '^' => {
            result.push(Terminal::Pow);
            scan_rec(&cs[1..], result)
        },        
        '/' => {
            result.push(Terminal::Div);
            scan_rec(&cs[1..], result)
        },
        '(' => {
            result.push(Terminal::Lpar);
            scan_rec(&cs[1..], result)
        },
        ')' => {
            result.push(Terminal::Rpar);
            scan_rec(&cs[1..], result)
        },
        '_' => {
            result.push(Terminal::Root);
            scan_rec(&cs[1..], result)
        },
        '-' => {
            //result.push(Terminal::Add);
            // TODO: this does not work!!w
            //result.push(Terminal::Float(-1.0));
            //result.push(Terminal::Mul);
            result.push(Terminal::Sub);
            scan_rec(&cs[1..], result)
        },
        c if is_digit(c) => {
            let (cs1, t) = scan_num(&cs[1..], int_val(c));
            result.push(t);
            scan_rec(cs1, result)
        },
        c if is_blank(c) => scan_rec(&cs[1..], result),
        c if is_letter(c) => {
            let (cs1, n) = scan_name(&cs[1..], c.to_string());
            result.push(Terminal::Var(n));
            scan_rec(cs1, result)
        },
        _ => panic!("scan: everything exploded between my hands!!!!")
    }
}

pub fn scan(s: String) -> Vec<Terminal> {
    let chars: Vec<_> = s.chars().collect();
    scan_rec(&chars, vec![])
}

/*
    Inserts multiply terminal between terms where it has been implicit in string
*/
fn insert_mult_rec(inp: &[Terminal], mut out: Vec<Terminal>) -> Vec<Terminal> {
    fn first_term(term: &Terminal) -> bool {
        match term {
            Terminal::Float(_)  => true,
            Terminal::Var(_)    => true,
            Terminal::Int(_)    => true,
            _                   => false,
        }
    }
    fn second_term(term: &Terminal) -> bool {
        match term {
            Terminal::Float(_)  => true,
            Terminal::Var(_)    => true,
            Terminal::Int(_)    => true,
            Terminal::Lpar      => true,
            _                   => false,
        }
    }
    if inp.len() == 0 {
        out
    } else if inp.len() == 1 {
        out.push(inp[0].clone());
        out
    }
    else if first_term(&inp[0]) && second_term(&inp[1]) {
        out.push(inp[0].clone());
        out.push(Terminal::Mul);
        insert_mult_rec(&inp[1..], out)
    } else {
        out.push(inp[0].clone());
        insert_mult_rec(&inp[1..], out)
    }
}

pub fn insert_mult(inp: Vec<Terminal>) -> Vec<Terminal> {
    insert_mult_rec(&inp, vec![])
}

/* 
    Grammar:
    E    = T Eopt .
    Eopt = "+" T Eopt | e .
    T    = F Topt .
    Topt = "*" F Topt | "/" F Topt | e .
    F    = P Fopt .
    Fopt = "^" Int | "_" Int | e .
    P    = Int [ Float | Var | "(" E ")" .
    e is the empty sequence.
*/
type ParseIntm<'a> = (&'a [Terminal], Expr);

fn e(ts: &[Terminal]) -> ParseIntm {
    eopt(t(ts))
}

fn eopt((ts, in_value): ParseIntm) -> ParseIntm {
    if ts.len() > 0 && ts[0] == Terminal::Add {
        let (ts1, tv) = t(&ts[1..]);
        eopt((ts1, Expr::Add(bx!(in_value), bx!(tv))))
    } else {
        (ts, in_value)
    }
}

fn t(ts: &[Terminal]) -> ParseIntm {
    topt(f(ts))
}

fn topt((ts, in_value): ParseIntm) -> ParseIntm {
    if ts.len() == 0 {
        return (ts, in_value);
    } 
    match ts[0] {
        Terminal::Mul => {
            let (ts1, fv) = f(&ts[1..]);
            topt((ts1, Expr::Mul(bx!(in_value), bx!(fv))))
        },
        Terminal::Div => {
            let (ts1, fv) = f(&ts[1..]);
            topt((ts1, Expr::Div(bx!(in_value), bx!(fv))))
        },
        _ => (ts, in_value)
    }
}

fn f(ts: &[Terminal]) -> ParseIntm {
    fopt(p(ts))
}

fn fopt((ts, in_value): ParseIntm) -> ParseIntm {
    if ts.len() < 2 {
        return (ts, in_value);
    } 
    match (&ts[0], &ts[1]) {
        (Terminal::Pow, Terminal::Int(i))   => 
            (&ts[2..], Expr::Exponent(bx!(in_value), *i)),
        (Terminal::Root, Terminal::Int(i))  => 
            (&ts[2..], Expr::Root(bx!(in_value), *i)),
        _  => (ts, in_value),
    }
}

// there is some potential for buggy grammar, if the original string is not well formulated
fn p(ts: &[Terminal]) -> ParseIntm {
    match &ts[0] {
        Terminal::Float(f)  => (&ts[1..], Expr::Num(*f)),
        Terminal::Int(i)    => (&ts[1..], Expr::Num(*i as f32)),
        Terminal::Var(x)    => (&ts[1..], Expr::Var(x.clone())),
        Terminal::Lpar      => {
            let (ts1, ev) = e(&ts[1..]);
            if ts1[0] == Terminal::Rpar {
                (&ts1[1..], ev)
            } else {
                panic!("P: should not be here")
            }
        },
        Terminal::Add       => p(&ts[1..]),
        _ => panic!("P: Could not parse terminal")
    }
}

fn parse_rec(ts: &[Terminal]) -> Expr {
    match e(ts) {
        (ts1, result) if ts1.len() == 0 => {
            result
        },
        _ => panic!("parse_rec: Bad stuff happened"),
    }
}

pub fn parse(ts: Vec<Terminal>) -> Expr {
    parse_rec(&ts)
}

/*
    Runs all the above functions for a string equation
    Returns an Expr
*/
pub fn parse_string(s: String) -> Expr {
    // scan:        scan string and create vector of terminals
    // insert_mult: add Terminal::Mul where it has been implicit in the string
    // parse:       finally transform the Terminals to a (potentially recursive) Expr
    parse(insert_mult(scan(s)))
}

/*
    Substitutes an Expr e, with another expression, ex, if the FVar's variable name matches x
*/
pub fn subst(e: &Expr, x: &str, exp: &Expr) -> Expr {
    match e {
        Expr::Var(s)            => if s == x { (*exp).clone() } 
                                   else { Expr::Var(s.into()) },
        Expr::Add(a, b)         => Expr::Add(
                                    bx!(subst(a, x, exp)),
                                    bx!(subst(b, x, exp))
                                   ),
        Expr::Mul(a, b)         => Expr::Mul(
                                    bx!(subst(a, x, exp)),
                                    bx!(subst(b, x, exp))
                                   ),
        Expr::Exponent(a, i)    => Expr::Exponent(
                                    bx!(subst(a, x, exp)),
                                    *i
                                   ),
        Expr::Div(a, b)         => Expr::Div(
                                    bx!(subst(a, x, exp)),
                                    bx!(subst(b, x, exp))
                                   ),
        Expr::Root(a, i)        => Expr::Root(
                                    bx!(subst(a, x, exp)),
                                    *i
                                   ),
        _                       => e.clone(), // FNum
    }
}

fn reduce_add(ex1: Expr, ex2: Expr, changed: bool) -> (Expr, bool) {
    match (ex1, ex2) {
        (ex1, Expr::Num(c)) if c == 0.0 => reduce_expr_rec(ex1, true),
        (Expr::Num(c), ex2) if c == 0.0 => reduce_expr_rec(ex2, true),
        (Expr::Num(c1), Expr::Num(c2)) => (Expr::Num(c1 + c2), true),
        (ex1, ex2) => {
            let (new_ex1, new_changed) = reduce_expr_rec(ex1, changed);
            let (new_ex2, new_new_changed) = reduce_expr_rec(ex2, new_changed);
            (Expr::Add(bx!(new_ex1), bx!(new_ex2)), new_new_changed)
        }
    }
}

fn reduce_mul(ex1: Expr, ex2: Expr, changed: bool) -> (Expr, bool) {
    match (ex1, ex2) {
        (_, Expr::Num(c)) if c == 0.0 => (Expr::Num(0.0), true),
        (Expr::Num(c), _) if c == 0.0 => (Expr::Num(0.0), true),
        (Expr::Num(c1), Expr::Num(c2)) => (Expr::Num(c1 * c2), true),
        (ex1, ex2) => {
            let (new_ex1, new_changed) = reduce_expr_rec(ex1, changed);
            let (new_ex2, new_new_changed) = reduce_expr_rec(ex2, new_changed);
            (Expr::Mul(bx!(new_ex1), bx!(new_ex2)), new_new_changed)
        }
    }
}

fn reduce_div(ex1: Expr, ex2: Expr, changed: bool) -> (Expr, bool) {
    match (ex1, ex2) {
        (_, Expr::Num(c)) if c == 0.0 => panic!("reduce_div: can't divide by zero"),
        (Expr::Num(c), _) if c == 0.0 => (Expr::Num(0.0), true),
        (Expr::Num(c1), Expr::Num(c2)) => (Expr::Num(c1 / c2), true),
        (ex1, ex2) => {
            let (new_ex1, new_changed) = reduce_expr_rec(ex1, changed);
            let (new_ex2, new_new_changed) = reduce_expr_rec(ex2, new_changed);
            (Expr::Div(bx!(new_ex1), bx!(new_ex2)), new_new_changed)
        }
    }
}

fn reduce_exp(ex1: Expr, n: i32, changed: bool) -> (Expr, bool) {
    match ex1 {
        Expr::Num(c) => (Expr::Num(c.powi(n)), true),
        _ => {
            let (new_ex1, new_changed) = reduce_expr_rec(ex1, changed);
            (Expr::Exponent(bx!(new_ex1), n), new_changed)
        }
    }
}

fn reduce_root(ex1: Expr, n: i32, changed: bool) -> (Expr, bool) {
    match ex1 {
        Expr::Num(c) => (Expr::Num(c.powf(1.0 / n as f32)), true),
        _ => {
            let (new_ex1, new_changed) = reduce_expr_rec(ex1, changed);
            (Expr::Root(bx!(new_ex1), n), new_changed)
        }
    }
}

fn reduce_expr_rec(exp: Expr, changed: bool) -> (Expr, bool) {
    match exp {
        Expr::Add(e1, e2) => reduce_add(*e1, *e2, changed),
        Expr::Mul(e1, e2) => reduce_mul(*e1, *e2, changed),
        Expr::Div(e1, e2) => reduce_div(*e1, *e2, changed),
        Expr::Exponent(e1, n) => reduce_exp(*e1, n, changed),
        Expr::Root(e1, n) => reduce_root(*e1, n, changed),
        _ => (exp, changed), // Var and Num
    }
}

/*
    Reduces all possible expressions that include numbers
*/
pub fn reduce_expr(exp: Expr) -> Expr {
    let (new_exp, changed) = reduce_expr_rec(exp, false);
    if changed {
        reduce_expr(new_exp)
    } else {
        new_exp
    }
}

/*
    Given a point, with values for x, y, and z, solves the expression
*/
pub fn solve_expr(exp: &Expr, p: &Point) -> f32 {
    match exp {
        Expr::Num(c)            => *c,
        Expr::Add(e1, e2)       => solve_expr(e1, p) + solve_expr(e2, p),
        Expr::Mul(e1, e2)       => solve_expr(e1, p) * solve_expr(e2, p),
        Expr::Div(e1, e2)       => solve_expr(e1, p) / solve_expr(e2, p),
        Expr::Exponent(e1, n)   => (solve_expr(e1, p)).powi(*n),
        Expr::Root(e1, n)       => (solve_expr(e1, p)).powf(1.0 / *n as f32),
        Expr::Var(s)            =>
            match s.as_str() {
                "x" => p.x,
                "y" => p.y,
                "z" => p.z,
                _ => panic!("solve_expr: unmatched variable"),
            },
    }
}