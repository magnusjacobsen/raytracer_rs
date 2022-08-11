#![feature(test)]
pub mod core;
pub mod implicit_surfaces;
pub mod util;
use implicit_surfaces::simple_expr::{Atom, combine};

fn main() {
    let xs = vec![vec![Atom::Exponent("x".into(), 2)], vec![Atom::Exponent("z".into(), 1)]];
    let ys = vec![vec![Atom::Exponent("y".into(), 1)], vec![Atom::Num(3.0)]];

    let expected = vec![vec![Atom::Exponent("x".into(), 2), Atom::Exponent("y".into(), 1)], vec![Atom::Exponent("z".into(), 1), Atom::Exponent("y".into(), 1)], vec![Atom::Exponent("x".into(), 2), Atom::Num(3.0)], vec![Atom::Exponent("z".into(), 1), Atom::Num(3.0)]];
    
    let res = combine(xs, ys);

    if expected == res {
        println!("equality!!!");
    } else {
        println!("not equal :(");
    }
    /*
    let eq = "x_3 * 4".to_string();
    let expr = implicit_surfaces::expr_parse::parse_string(eq);
    println!("Hello, world!\n{:?}", expr);
    */
}
