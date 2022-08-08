#![feature(test)]
pub mod core;
pub mod implicit_surfaces;

fn main() {
    let eq = "y^-4 * x_3".to_string();
    let expr = implicit_surfaces::expr_parse::parse_string(eq);
    println!("Hello, world!\n{:?}", expr);
}
