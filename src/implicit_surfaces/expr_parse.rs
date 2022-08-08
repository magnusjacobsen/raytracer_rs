/*
    This module is able to scan and parse an equation string,
    and turn it into a expr format.
*/

// omg we have discriminated unions, like in F# :O
#[derive(PartialEq, Clone, Debug)]
enum Terminal {
    Add,
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

#[derive(Debug)]
pub enum Expr {
    FNum(f32),
    FVar(String),
    FAdd(Box<Expr>, Box<Expr>),
    FMult(Box<Expr>, Box<Expr>),
    FDiv(Box<Expr>, Box<Expr>),
    FExponent(Box<Expr>, i32),
    FRoot(Box<Expr>, i32),
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

fn _negate(t: Terminal) -> Terminal {
    match t {
        Terminal::Int(i)    => Terminal::Int(-i),
        Terminal::Float(f)  => Terminal::Float(-f),
        _                   => panic!("Expected a Terminal of Float or Int")
    }
}

/*************
    All the various scanner sub functions
***************/

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

fn scan(cs: &[char], result: &mut Vec<Terminal>) {
    if cs.len() == 0 {
        return;
    }

    match cs[0] {
        '+' => {
            result.push(Terminal::Add);
            scan(&cs[1..], result);
        },
        '*' => {
            result.push(Terminal::Mul);
            scan(&cs[1..], result);
        },
        '^' => {
            result.push(Terminal::Pow);
            scan(&cs[1..], result);
        },        
        '/' => {
            result.push(Terminal::Div);
            scan(&cs[1..], result);
        },
        '(' => {
            result.push(Terminal::Lpar);
            scan(&cs[1..], result);
        },
        ')' => {
            result.push(Terminal::Rpar);
            scan(&cs[1..], result);
        },
        '_' => {
            result.push(Terminal::Root);
            scan(&cs[1..], result);
        },
        '-' => {
            result.push(Terminal::Add);
            // TODO: this does not work!!w
            result.push(Terminal::Float(-1.0));
            result.push(Terminal::Mul);
            scan(&cs[1..], result);
        },
        c if is_digit(c) => {
            let (cs1, t) = scan_num(&cs[1..], int_val(c));
            result.push(t);
            scan(cs1, result);
        },
        c if is_blank(c) => scan(&cs[1..], result),
        c if is_letter(c) => {
            let (cs1, n) = scan_name(&cs[1..], c.to_string());
            result.push(Terminal::Var(n));
            scan(cs1, result);
        },
        _ => panic!("scan: everything exploded between my hands!!!!")
    }
}

/*
    Inserts multiply terminal between terms where it has been implicit in string
*/
fn insert_mult(inp: &[Terminal], result: &mut Vec<Terminal>) {
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
        return;
    } else if inp.len() == 1 {
        result.push(inp[0].clone());
        return;
    }
    else if first_term(&inp[0]) && second_term(&inp[1]) {
        result.push(inp[0].clone());
        result.push(Terminal::Mul);
        insert_mult(&inp[1..], result);
    } else {
        result.push(inp[0].clone());
        insert_mult(&inp[1..], result);
    }
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
        eopt((ts1, Expr::FAdd(Box::new(in_value), Box::new(tv))))
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
            topt((ts1, Expr::FMult(
                Box::new(in_value), Box::new(fv))))
        },
        Terminal::Div => {
            let (ts1, fv) = f(&ts[1..]);
            topt((ts1, Expr::FDiv(
                Box::new(in_value), Box::new(fv))))
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
            (&ts[2..], Expr::FExponent(Box::new(in_value), *i)),
        (Terminal::Root, Terminal::Int(i))  => 
            (&ts[2..], Expr::FRoot(Box::new(in_value), *i)),
        _  => (ts, in_value),
    }
}

// there is some potential for buggy grammar, if the original string is not well formulated
fn p(ts: &[Terminal]) -> ParseIntm {
    match &ts[0] {
        Terminal::Float(f)  => (&ts[1..], Expr::FNum(*f)),
        Terminal::Int(i)    => (&ts[1..], Expr::FNum(*i as f32)),
        Terminal::Var(x)    => (&ts[1..], Expr::FVar(x.clone())),
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

fn parse(ts: &[Terminal]) -> Result<Expr, &str> {
    match e(ts) {
        (ts1, result) if ts1.len() == 0 => {
            Ok(result)
        },
        _ => Err("parse: bad stuff happened!"),
    }
}

/*
    Runs all the above functions for a string equation
    Returns an Expr
*/
pub fn parse_string(s: String) -> Expr {
    // scan string and create vector of terminals
    let mut terminals = Vec::new();
    let cs = s.chars().collect::<Vec<_>>();
    scan(&cs[..], &mut terminals);
    
    println!("{:?}", terminals);
    
    // add Terminal::Mul where it has been implicit in the string
    let mut terms_added_mult = Vec::new();
    insert_mult(&terminals[..], &mut terms_added_mult);

    // finally transform the Terminals to a (potentially recursive) Expr
    parse(&terms_added_mult).expect("parse_string: could not parse")
}