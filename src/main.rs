#![feature(test, type_alias_impl_trait)]
pub mod core;
pub mod implicit_surfaces;
pub mod util;
use implicit_surfaces::{implicit_surfaces::make_implicit, expr::{parse_string, Expr, subst}};

use crate::implicit_surfaces::{simple_expr::expr_to_simple_expr, poly::expr_to_poly};

fn main() {
    let sphere = "(x^2 + y^2 + z^2)".to_string();
    let sphere_expr = parse_string(sphere);
    let ex = Expr::Add(
        bx!(Expr::Var("px".into())),
        bx!(Expr::Mul(
            bx!(Expr::Var("t".into())),
            bx!(Expr::Var("dx".into()))
        ))
    );
    let ey = Expr::Add(
        bx!(Expr::Var("py".into())),
        bx!(Expr::Mul(
            bx!(Expr::Var("t".into())),
            bx!(Expr::Var("dy".into()))
        ))
    );
    let ez = Expr::Add(
        bx!(Expr::Var("pz".into())),
        bx!(Expr::Mul(
            bx!(Expr::Var("t".into())),
            bx!(Expr::Var("dz".into()))
        ))
    );
    //let er = Expr::Num(-1.0);
    let sub_vec = vec![("x", ex), ("y", ey), ("z", ez)];

    let expr_sub = sub_vec.iter()
        .fold(sphere_expr, |acc, (var, sub)| subst(acc, var, sub));

    let poly = expr_to_poly(expr_sub, "t".to_string());


    println!("result_se: {:?}", poly);
}
