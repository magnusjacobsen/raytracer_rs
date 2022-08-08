#![feature(test)]
pub mod core;
pub mod implicit_surfaces;
pub mod util;

fn main() {
    let eq = "-2*-3 + 7".to_string();
    let expr = implicit_surfaces::expr_parse::parse_string(eq);
    println!("Hello, world!\n{:?}", expr);
}
