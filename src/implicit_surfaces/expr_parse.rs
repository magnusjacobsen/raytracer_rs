/*
    This module is able to scan and parse an equation string,
    and turn it into a expr format.
*/

// omg we have discriminated unions, like in F# :O
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

// helper functions
fn is_blank(c: char) -> bool {
    c.is_whitespace()
}

fn is_digit(c: char) -> bool {
    c.is_digit()
}

fn is_letter(c: char) -> bool {
    c.is_alphabetic()
}

fn is_letter_digit(c: char) -> bool {
    c.is_alphanumeric()
}

// create a list from a sequence
fn to_list<T>(iter: Iter<T>) -> Vec<T> {
    iter.collect()
}

fn int_val(c: char) -> i32 {
    c.to_digit(10)
}

fn float_val(c: char) -> f32 {
    int_val(c) as f32
}

fn negate(t: Terminal) -> Result<Terminal> {
    match t {
        Int(i)      => Ok(Int(-i)),
        Float(f)    => Ok(Float(-f)),
        _           => Err("Expected a Terminal of Float or Int")
    }
}

/*************
    All the various scanner sub functions
***************/

/*
    Scans for a Float
*/
fn scan_frac(cs: &[char], value: f32, wt: f32) -> (&[char], f32) {
    if cs.len() > 0 && is_digit(cs[0]) {
        scan_frac(&cs[1..], value + wt * float_val(cs[0], wt / 10.0))
    }
    (cs, Terminal::Float(value))
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

fn scan_name(cs: &[char], value: String) -> (&[char], Terminal) {
    if cs.len() > 0 && is_letter_digit(cs[0]) {
        scan_name(&cs[1..], value + cs[0].to_string())
    }
    (cs, value)
}

fn scan(s: &[char], result: &mut Vec<Terminal>) {
    if s.len() == 0 {
        return;
    }

    match s[0] {
        '+' => {
            result.push(Terminal::Add);
            scan(&s[1..], result);
        },
        '*' => {
            result.push(Terminal::Mul);
            scan(&s[1..], result);
        },
        '^' => {
            result.push(Terminal::Pow);
            scan(&s[1..], result);
        },        
        '/' => {
            result.push(Terminal::Div);
            scan(&s[1..], result);
        },
        '(' => {
            result.push(Terminal::Lpar);
            scan(&s[1..], result);
        },
        ')' => {
            result.push(Terminal::Rpar);
            scan(&s[1..], result);
        },
        '_' => {
            result.push(Terminal::Root);
            scan(&s[1..], result);
        },
        '-' => {
            result.push(Terminal::Add);
            result.push(Terminal::Float(-1));
            result.push(Terminal::Mul);
            scan(&s[1..], result);
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