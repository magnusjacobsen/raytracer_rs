#[cfg(test)]

pub mod t {
    extern crate test;
    use std::io::Result;

    use crate::bx;
    use crate::implicit_surfaces::simple_expr::{Atom, combine, self, expr_to_simple_expr, SimpleExpr};
    use crate::implicit_surfaces::expr::{self, Expr, parse_string, subst};

    fn sort_simple_expr(mut se: SimpleExpr) -> SimpleExpr {
        let mut out = vec![];
        for mut atom_group in se {
            atom_group.sort_by(|a,b| a.partial_cmp(&b).unwrap());
            out.push(atom_group);
        }
        out.sort_by(|a,b| a.partial_cmp(&b).unwrap());
        out
    }

    #[test]
    fn combine_t1() -> Result<()> {
        /*
            combine(xs = x^2 + z, ys = y + 3)
            result = (x^2 * y) + (x^2 * 3) + (z * y) + (z * 3)
        */
        let xs = vec![vec![Atom::Exponent("x".into(), 2)], vec![Atom::Exponent("z".into(), 1)]];
        let ys = vec![vec![Atom::Exponent("y".into(), 1)], vec![Atom::Num(3.0)]];

        let expected = vec![vec![Atom::Exponent("x".into(), 2), Atom::Exponent("y".into(), 1)], vec![Atom::Exponent("x".into(), 2), Atom::Num(3.0)], vec![Atom::Exponent("z".into(), 1), Atom::Exponent("y".into(), 1)], vec![Atom::Exponent("z".into(), 1), Atom::Num(3.0)]];
        let result = combine(&xs, &ys);

        assert_eq!(result, expected);
        
        Ok(())
    }

    #[test]
    fn combine_t2() -> Result<()> {
        /*
            combine(xs = x^2 + z, ys = y + 3)
            result = (x^2 * y) + (x^2 * 3) + (z * y) + (z * 3)
        */
        let xs = vec![vec![Atom::Exponent("x".into(), 2)], vec![Atom::Exponent("z".into(), 1)]];
        let ys = vec![vec![]];

        let expected = vec![vec![Atom::Exponent("x".into(), 2)], vec![Atom::Exponent("z".into(), 1)]];
        let result = combine(&xs, &ys);

        assert_eq!(result, expected);
        
        Ok(())
    }

    #[test]
    fn simplify_atom_group() -> Result<()> {
        let input = vec![
            Atom::Exponent("px".into(), 1),
            Atom::Exponent("px".into(), 2),
            Atom::Num(-2.0),
            Atom::Num(-2.0),

        ];
        let result = simple_expr::simplify_atom_group(&input);
        let expected = vec![
            Atom::Num(4.0),
            Atom::Exponent("px".into(), 3)
        ];
        assert_eq!(result, expected);
        Ok(())
    }

    #[test]
    fn simplify_simple_expr() -> Result<()> {
        let input = vec![vec![
            Atom::Num(3.0)
        ], vec![
            Atom::Num(4.0)
        ], vec![
            Atom::Exponent("x".into(), 2),
            Atom::Exponent("y".into(), 3)
        ], vec![
            Atom::Exponent("x".into(), 2),
            Atom::Exponent("y".into(), 3)
        ]];
        let result = simple_expr::simplify_simple_expr(input);
        let expected = vec![vec![
            Atom::Num(2.0),
            Atom::Exponent("x".into(), 2),
            Atom::Exponent("y".into(), 3)
        ],
        vec![
            Atom::Num(7.0)
        ]];

        assert_eq!(result, expected);
        Ok(())
    }

    #[test]
    fn parse_to_simple_expr() -> Result<()> {
        let input = "a*b+b*a".into();
        let result = simple_expr::simplify_simple_expr(simple_expr::expr_to_simple_expr(expr::parse_string(input)));
        let expected = vec![vec![
            Atom::Num(2.0),
            Atom::Exponent("a".into(), 1),
            Atom::Exponent("b".into(), 1)
        ]];

        assert_eq!(result, expected);
        Ok(())
    }

    #[test]
    fn subst_to_simple_expr() -> Result<()> {
        let sphere = "(x^2 + y^2 + z^2 - R)".to_string();
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
        let er = Expr::Num(-1.0);
        let sub_vec = vec![("x", ex), ("y", ey), ("z", ez), ("R", er)];

        let result_sub = sub_vec.iter()
            .fold(sphere_expr, |acc, (var, sub)| subst(acc, var, sub));

        let mut result_se = expr_to_simple_expr(result_sub.clone());

        let expected_sub = 
            Expr::Add(
                bx!(Expr::Add(
                    bx!(Expr::Add(
                        bx!(Expr::Exponent(
                            bx!(Expr::Add(
                                bx!(Expr::Var("px".into())),
                                bx!(Expr::Mul(
                                    bx!(Expr::Var("t".into())),
                                    bx!(Expr::Var("dx".into()))
                                ))
                            )),
                            2
                        )),
                        bx!(Expr::Exponent(
                            bx!(Expr::Add(
                                bx!(Expr::Var("py".into())),
                                bx!(Expr::Mul(
                                    bx!(Expr::Var("t".into())),
                                    bx!(Expr::Var("dy".into()))
                                ))
                            )),
                            2
                        ))
                    )),
                    bx!(Expr::Exponent(
                        bx!(Expr::Add(
                            bx!(Expr::Var("pz".into())),
                            bx!(Expr::Mul(
                                bx!(Expr::Var("t".into())),
                                bx!(Expr::Var("dz".into()))
                            ))
                        )),
                        2
                    )),
                )),
                bx!(Expr::Mul(
                    bx!(Expr::Num(-1.0)),
                    bx!(Expr::Num(-1.0))
                ))
            );
        let expected_se = vec![
            vec![Atom::Num(1.0)],
            vec![Atom::Num(2.0), Atom::Exponent("dx".into(), 1), Atom::Exponent("px".into(), 1), Atom::Exponent("t".into(), 1)],
            vec![Atom::Num(2.0), Atom::Exponent("dy".into(), 1), Atom::Exponent("py".into(), 1), Atom::Exponent("t".into(), 1)],
            vec![Atom::Num(2.0), Atom::Exponent("dz".into(), 1), Atom::Exponent("pz".into(), 1), Atom::Exponent("t".into(), 1)],
            vec![Atom::Exponent("dx".into(), 2), Atom::Exponent("t".into(), 2)],
            vec![Atom::Exponent("dy".into(), 2), Atom::Exponent("t".into(), 2)],
            vec![Atom::Exponent("dz".into(), 2), Atom::Exponent("t".into(), 2)],
            vec![Atom::Exponent("px".into(), 2)],
            vec![Atom::Exponent("py".into(), 2)],
            vec![Atom::Exponent("pz".into(), 2)],
        ];

        result_se = sort_simple_expr(result_se);
        //expected_se = sort_simple_expr(expected_se);

        assert_eq!(result_sub, expected_sub);
        assert_eq!(result_se, expected_se);

        Ok(())
    }
}