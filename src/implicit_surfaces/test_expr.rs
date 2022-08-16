#[cfg(test)]

pub mod t {
    extern crate test;
    use std::io::Result;

    use crate::implicit_surfaces::expr::{Terminal, Expr, scan, insert_mult, parse_string};
    use crate::bx;

    #[test]
    fn test_scan() -> Result<()> {
        let test_list = vec![
            ("x_3 * 4", vec![Terminal::Var("x".into()), Terminal::Root, Terminal::Int(3), Terminal::Mul, Terminal::Int(4)]),
            ("2*3", vec![Terminal::Int(2), Terminal::Mul, Terminal::Int(3)]),
            ("-2*3", vec![Terminal::Int(-2), Terminal::Mul, Terminal::Int(3)]),
            ("-2*-3", vec![Terminal::Int(-2), Terminal::Mul, Terminal::Int(-3)]),
            ("2+3*4", vec![Terminal::Int(2), Terminal::Add, Terminal::Int(3), Terminal::Mul, Terminal::Int(4)]),
            ("2+-3*-4", vec![Terminal::Int(2), Terminal::Add, Terminal::Int(-3), Terminal::Mul, Terminal::Int(-4)]),
            ("(2.0+3)*4", vec![Terminal::Lpar, Terminal::Float(2.0), Terminal::Add, Terminal::Int(3), Terminal::Rpar, Terminal::Mul, Terminal::Int(4)]),
            ("-1(2+3)", vec![Terminal::Int(-1), Terminal::Lpar, Terminal::Int(2), Terminal::Add, Terminal::Int(3), Terminal::Rpar]),
            ("2^4", vec![Terminal::Int(2), Terminal::Pow, Terminal::Int(4)]),
            ("x^2", vec![Terminal::Var("x".into()), Terminal::Pow, Terminal::Int(2)]),
            ("x2", vec![Terminal::Var("x2".into())]),
            ("2x", vec![Terminal::Int(2), Terminal::Var("x".into())]),
            ("-1x", vec![Terminal::Int(-1), Terminal::Var("x".into())]),
            ("2 x", vec![Terminal::Int(2), Terminal::Var("x".into())]),
            ("2 x y", vec![Terminal::Int(2), Terminal::Var("x".into()), Terminal::Var("y".into())]),
            ("2xy", vec![Terminal::Int(2), Terminal::Var("xy".into())]),
            ("2x(2x)", vec![Terminal::Int(2), Terminal::Var("x".into()), Terminal::Lpar, Terminal::Int(2), Terminal::Var("x".into()), Terminal::Rpar]),
            ("2 x 2 y(2 x(-2))", vec![Terminal::Int(2), Terminal::Var("x".into()), Terminal::Int(2), Terminal::Var("y".into()), Terminal::Lpar, Terminal::Int(2), Terminal::Var("x".into()), Terminal::Lpar, Terminal::Int(-2), Terminal::Rpar, Terminal::Rpar]),
            ("2x^2*2y^2", vec![Terminal::Int(2), Terminal::Var("x".into()), Terminal::Pow, Terminal::Int(2), Terminal::Mul, Terminal::Int(2), Terminal::Var("y".into()), Terminal::Pow, Terminal::Int(2)]),
            ("2x^2(2y^2)", vec![Terminal::Int(2), Terminal::Var("x".into()), Terminal::Pow, Terminal::Int(2), Terminal::Lpar, Terminal::Int(2), Terminal::Var("y".into()), Terminal::Pow, Terminal::Int(2), Terminal::Rpar]),
        ];

        test_list.iter().for_each(|(inp, expected)| {
            let result = scan(inp.to_string());
            assert_eq!(result, *expected);
        });

        Ok(())
    }

    #[test]
    fn test_insert_mult() -> Result<()> {
        let test_list = vec![
            ("x_3 * 4", vec![Terminal::Var("x".into()), Terminal::Root, Terminal::Int(3), Terminal::Mul, Terminal::Int(4)]),
            ("2*3", vec![Terminal::Int(2), Terminal::Mul, Terminal::Int(3)]),
            ("-2*3", vec![Terminal::Int(-2), Terminal::Mul, Terminal::Int(3)]),
            ("-2*-3", vec![Terminal::Int(-2), Terminal::Mul, Terminal::Int(-3)]),
            ("2+3*4", vec![Terminal::Int(2), Terminal::Add, Terminal::Int(3), Terminal::Mul, Terminal::Int(4)]),
            ("2+-3*-4", vec![Terminal::Int(2), Terminal::Add, Terminal::Int(-3), Terminal::Mul, Terminal::Int(-4)]),
            ("(2.0+3)*4", vec![Terminal::Lpar, Terminal::Float(2.0), Terminal::Add, Terminal::Int(3), Terminal::Rpar, Terminal::Mul, Terminal::Int(4)]),
            ("-1(2+3)", vec![Terminal::Int(-1), Terminal::Mul, Terminal::Lpar, Terminal::Int(2), Terminal::Add, Terminal::Int(3), Terminal::Rpar]),
            ("2^4", vec![Terminal::Int(2), Terminal::Pow, Terminal::Int(4)]),
            ("x^2", vec![Terminal::Var("x".into()), Terminal::Pow, Terminal::Int(2)]),
            ("x2", vec![Terminal::Var("x2".into())]),
            ("2x", vec![Terminal::Int(2), Terminal::Mul, Terminal::Var("x".into())]),
            ("-1x", vec![Terminal::Int(-1), Terminal::Mul, Terminal::Var("x".into())]),
            ("2 x", vec![Terminal::Int(2), Terminal::Mul, Terminal::Var("x".into())]),
            ("2 x y", vec![Terminal::Int(2), Terminal::Mul, Terminal::Var("x".into()), Terminal::Mul, Terminal::Var("y".into())]),
            ("2xy", vec![Terminal::Int(2), Terminal::Mul, Terminal::Var("xy".into())]),
            ("2x(2x)", vec![Terminal::Int(2), Terminal::Mul, Terminal::Var("x".into()), Terminal::Mul, Terminal::Lpar, Terminal::Int(2), Terminal::Mul, Terminal::Var("x".into()), Terminal::Rpar]),
            ("2 x 2 y(2 x(-2))", vec![Terminal::Int(2), Terminal::Mul, Terminal::Var("x".into()), Terminal::Mul, Terminal::Int(2), Terminal::Mul, Terminal::Var("y".into()), Terminal::Mul, Terminal::Lpar, Terminal::Int(2), Terminal::Mul, Terminal::Var("x".into()), Terminal::Mul, Terminal::Lpar, Terminal::Int(-2), Terminal::Rpar, Terminal::Rpar]),
            ("2x^2*2y^2", vec![Terminal::Int(2), Terminal::Mul, Terminal::Var("x".into()), Terminal::Pow, Terminal::Int(2), Terminal::Mul, Terminal::Int(2), Terminal::Mul, Terminal::Var("y".into()), Terminal::Pow, Terminal::Int(2)]),
            ("2x^2(2y^2)", vec![Terminal::Int(2), Terminal::Mul, Terminal::Var("x".into()), Terminal::Pow, Terminal::Int(2), Terminal::Mul, Terminal::Lpar, Terminal::Int(2), Terminal::Mul, Terminal::Var("y".into()), Terminal::Pow, Terminal::Int(2), Terminal::Rpar]),
        ];

        test_list.iter().for_each(|(inp, expected)| {
            let result = insert_mult(scan(inp.to_string()));
            assert_eq!(result, *expected);
        });

        Ok(())
    }

    #[test]
    fn test_parse_string() -> Result<()> {
        let test_list = [
            ("x_3 * 4", 
                Expr::Mul(
                    bx!(Expr::Root(
                        bx!(Expr::Var("x".into())),
                        3   
                    )),
                    bx!(Expr::Num(4.0))
                )
            ),
            ("2*3", 
                Expr::Mul(
                    bx!(Expr::Num(2.0)), 
                    bx!(Expr::Num(3.0))
                )
            ),
            ("-2*3", 
                Expr::Mul(
                    bx!(Expr::Num(-2.0)), 
                    bx!(Expr::Num(3.0))
                )
            ),
            ("-2*-3", 
                Expr::Mul(
                    bx!(Expr::Num(-2.0)), 
                    bx!(Expr::Num(-3.0))
                )
            ),
            ("2+3*4", 
                Expr::Add(
                    bx!(Expr::Num(2.0)), 
                    bx!(Expr::Mul(
                        bx!(Expr::Num(3.0)), 
                        bx!(Expr::Num(4.0))
                    ))
                )
            ),
            ("2+-3*-4", 
                Expr::Add(
                    bx!(Expr::Num(2.0)), 
                    bx!(Expr::Mul(
                        bx!(Expr::Num(-3.0)),
                        bx!(Expr::Num(-4.0))
                    ))
                )
            ),
            ("(2.0+3)*4", 
                Expr::Mul(
                    bx!(Expr::Add(
                        bx!(Expr::Num(2.0)), 
                        bx!(Expr::Num(3.0))
                    )), 
                    bx!(Expr::Num(4.0))
                )
            ),
            ("-1(2+3)",
                Expr::Mul(
                    bx!(Expr::Num(-1.0)),
                    bx!(Expr::Add(
                        bx!(Expr::Num(2.0)),
                        bx!(Expr::Num(3.0))
                    ))
                )
            ),
            ("2^4",
                Expr::Exponent(
                    bx!(Expr::Num(2.0)),
                    4
                )
            ),
            ("x^2",
                Expr::Exponent(
                    bx!(Expr::Var("x".into())),
                    2
                )
            ),
            ("x2", Expr::Var("x2".into())),
            ("2x",
                Expr::Mul(
                    bx!(Expr::Num(2.0)),
                    bx!(Expr::Var("x".into()))
                )
            ),
            ("-1x",
                Expr::Mul(
                    bx!(Expr::Num(-1.0)),
                    bx!(Expr::Var("x".into()))
                )
            ), 
            ("2 x",
                Expr::Mul(
                    bx!(Expr::Num(2.0)),
                    bx!(Expr::Var("x".into()))
                )
            ),
            ("2 x y",
                Expr::Mul(
                    bx!(Expr::Mul(
                        bx!(Expr::Num(2.0)),
                        bx!(Expr::Var("x".into()))
                    )),
                    bx!(Expr::Var("y".into()))
                )
            ),
            ("2xy",
                Expr::Mul(
                    bx!(Expr::Num(2.0)),
                    bx!(Expr::Var("xy".into()))
                )
            ),
            ("2x(2x)",
                Expr::Mul(
                    bx!(Expr::Mul(
                        bx!(Expr::Num(2.0)),
                        bx!(Expr::Var("x".into()))
                    )),
                    bx!(Expr::Mul(
                        bx!(Expr::Num(2.0)),
                        bx!(Expr::Var("x".into()))
                    ))
                )
            ),
            ("2 x 2 y(2 x(-2))",
                Expr::Mul(
                    bx!(Expr::Mul(
                        bx!(Expr::Mul(
                            bx!(Expr::Mul(
                                bx!(Expr::Num(2.0)),
                                bx!(Expr::Var("x".into()))
                            )),
                            bx!(Expr::Num(2.0))
                        )),
                        bx!(Expr::Var("y".into()))
                    )),
                    bx!(Expr::Mul(
                        bx!(Expr::Mul(
                            bx!(Expr::Num(2.0)),
                            bx!(Expr::Var("x".into()))
                        )),
                        bx!(Expr::Num(-2.0))
                    ))
                )
            ),
            ("2x^2*2y^2",
                Expr::Mul(
                    bx!(Expr::Mul(
                        bx!(Expr::Mul(
                            bx!(Expr::Num(2.0)),
                            bx!(Expr::Exponent(
                                bx!(Expr::Var("x".into())),
                                2
                            ))
                        )),
                        bx!(Expr::Num(2.0))
                    )),
                    bx!(Expr::Exponent(
                        bx!(Expr::Var("y".into())),
                        2
                    ))
                )
            ),
            ("2x^2(2y^2)",
                Expr::Mul(
                    bx!(Expr::Mul(
                        bx!(Expr::Num(2.0)),
                        bx!(Expr::Exponent(
                            bx!(Expr::Var("x".into())),
                            2
                        ))
                        )),
                    bx!(Expr::Mul(
                        bx!(Expr::Num(2.0)),
                        bx!(Expr::Exponent(
                            bx!(Expr::Var("y".into())),
                            2
                        ))
                    ))
                )
            )
        ];

        test_list.iter().for_each(|(inp, expected)| {
            let result = parse_string(inp.to_string());
            assert_eq!(result, *expected);
        });

        Ok(())
    }
}