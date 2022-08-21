#[cfg(test)]

pub mod t {
    extern crate test;
    use std::io::Result;

    use crate::bx;
    use crate::implicit_surfaces::simple_expr::{Atom, combine, self, expr_to_simple_expr, SimpleExpr, rewrite_expr};
    use crate::implicit_surfaces::expr::{self, Expr, parse_string, subst};

    fn sort_simple_expr(se: SimpleExpr) -> SimpleExpr {
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
            .fold(sphere_expr, |acc, (var, sub)| subst(&acc, var, sub));

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
        let mut expected_se = vec![
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
        expected_se = sort_simple_expr(expected_se);

        assert_eq!(result_sub, expected_sub);
        assert_eq!(result_se, expected_se);

        Ok(())
    }

    /*
        All the following tests are based on the rules listed on page 41 in the Lecture notes (updated April 14) document
    */

    #[test]
    fn rule_zero_degree_exponent() -> Result<()> {
        // case 1
        let input = "e^0".to_string();
        let result = rewrite_expr(parse_string(input));
        let expected = vec![vec![
            Atom::Num(1.0)
        ]];

        assert_eq!(result, expected);
        Ok(())
    }

    #[test]
    fn rule_one_degree_exponent() -> Result<()> {
        // case 2
        let input = "e^1".to_string();
        let result = rewrite_expr(parse_string(input));
        let expected = vec![vec![
            Atom::Exponent("e".into(), 1)
        ]];

        assert_eq!(result, expected);
        Ok(())
    }

    #[test]
    fn rule_third_degree_exponent() -> Result<()> {
        // case 3
        let input = "e^3".to_string();
        let result = rewrite_expr(parse_string(input));
        let expected = vec![vec![
            Atom::Exponent("e".into(), 1), 
            Atom::Exponent("e".into(), 1), 
            Atom::Exponent("e".into(), 1)
        ]];

        assert_eq!(result, expected);
        Ok(())
    }

    #[test]
    fn rule_multiply_plus_paranthesis() -> Result<()> {
        // case 4
        let input = "e1*(e2 + e3)".to_string();
        let result = sort_simple_expr(rewrite_expr(parse_string(input)));
        let expected = sort_simple_expr(vec![
            vec![
                Atom::Exponent("e2".into(), 1),
                Atom::Exponent("e1".into(), 1)
            ],
            vec![
                Atom::Exponent("e3".into(), 1),
                Atom::Exponent("e1".into(), 1)
            ]
        ]);

        assert_eq!(result, expected);
        Ok(())
    }

    #[test]
    fn rule_plus_paranthesis_multiply() -> Result<()> {
        // case 4
        let input = "(e2 + e3) * e1".to_string();
        let result = sort_simple_expr(rewrite_expr(parse_string(input)));
        let expected = sort_simple_expr(vec![
            vec![
                Atom::Exponent("e2".into(), 1),
                Atom::Exponent("e1".into(), 1)
            ],
            vec![
                Atom::Exponent("e3".into(), 1),
                Atom::Exponent("e1".into(), 1)
            ]
        ]);

        assert_eq!(result, expected);
        Ok(())    
    }

    #[test]
    fn rule_multiply_neg_paranthesis() -> Result<()> {
        // case 5
        let input = "e1 * (e2 - e3)".to_string();
        let result = sort_simple_expr(rewrite_expr(parse_string(input)));
        let expected = sort_simple_expr(vec![
            vec![
                Atom::Exponent("e2".into(), 1),
                Atom::Exponent("e1".into(), 1),
            ],
            vec![
                Atom::Exponent("e3".into(), 1),
                Atom::Num(-1.0),
                Atom::Exponent("e1".into(), 1)
            ]
        ]);

        assert_eq!(result, expected);
        Ok(())
    }

    #[test]
    fn rule_neg_paranthesis_multiply() -> Result<()> {
        // case 5
        let input = "(e2 - e3) * e1".to_string();
        let result = sort_simple_expr(rewrite_expr(parse_string(input)));
        let expected = sort_simple_expr(vec![
            vec![
                Atom::Exponent("e2".into(), 1),
                Atom::Exponent("e1".into(), 1),
            ],
            vec![
                Atom::Exponent("e3".into(), 1),
                Atom::Num(-1.0),
                Atom::Exponent("e1".into(), 1)
            ]
        ]);

        assert_eq!(result, expected);
        Ok(())
    }

    #[test]
    fn rule_multiply_division() -> Result<()> {
        // case 6
        let input = "e1 * (e2 / (e3 + 1))".to_string();
        let result = sort_simple_expr(rewrite_expr(parse_string(input)));
        let expected = sort_simple_expr(vec![vec![
            Atom::Exponent("e2".into(), 1),
            Atom::Exponent("e1".into(), 1)
        ]]);

        assert_eq!(result, expected);
        Ok(())
    }

    #[test]
    fn rule_division_twice() -> Result<()> {
        // case 7
        let input = "(e1 / e2) / e3".to_string();
        let result = sort_simple_expr(rewrite_expr(parse_string(input)));
        let expected = sort_simple_expr(vec![vec![
            Atom::Exponent("e1".into(), 1)
        ]]);

        assert_eq!(result, expected);
        Ok(())
    }

    #[test]
    fn rule_division_twice_other_way() -> Result<()> {
        // case 8
        let input = "e1 / (e2 / e3)".to_string();
        let result = sort_simple_expr(rewrite_expr(parse_string(input)));
        let expected = sort_simple_expr(vec![vec![
            Atom::Exponent("e3".into(), 1),
            Atom::Exponent("e1".into(), 1)
        ]]);

        assert_eq!(result, expected);
        Ok(())
    }

    #[test]
    fn rule_plus_division() -> Result<()> {
        // case 9
        let input = "e1 + (e2 / e3)".to_string();
        let result = sort_simple_expr(rewrite_expr(parse_string(input)));
        let expected = sort_simple_expr(vec![
            vec![
                Atom::Exponent("e3".into(), 1),
                Atom::Exponent("e1".into(), 1)
            ],
            vec![Atom::Exponent("e2".into(), 1)]
        ]);

        assert_eq!(result, expected);
        Ok(())
    }

    #[test]
    fn rule_plus_division_other_way() -> Result<()> {
        // case 9
        let input = "(e2 / e3) + e1".to_string();
        let result = sort_simple_expr(rewrite_expr(parse_string(input)));
        let expected = sort_simple_expr(vec![
            vec![
                Atom::Exponent("e3".into(), 1),
                Atom::Exponent("e1".into(), 1)
            ],
            vec![Atom::Exponent("e2".into(), 1)]
        ]);

        assert_eq!(result, expected);
        Ok(())
    }

    #[test]
    fn rule_division_of_divisions() -> Result<()> {
        // case 10
        let input = "(e1 / e2) / (e3 / e4)".to_string();
        let result = sort_simple_expr(rewrite_expr(parse_string(input)));
        let expected = sort_simple_expr(vec![vec![
            Atom::Exponent("e4".into(), 1),
            Atom::Exponent("e1".into(), 1)
        ]]);

        assert_eq!(result, expected);
        Ok(())
    }

    #[test]
    fn rule_root_multiples() -> Result<()> {
        // case 11
        let input = "e_3 * e_3 * e_3".to_string();
        let result = sort_simple_expr(rewrite_expr(parse_string(input)));
        let expected = vec![vec![Atom::Exponent("e".into(), 1)]];

        assert_eq!(result, expected);
        Ok(())
    }

    #[test]
    fn rule_root_fun_stuff() -> Result<()> {
        // also case 11
        let input = "e1_2 * e2_3".to_string();
        let result = sort_simple_expr(rewrite_expr(parse_string(input)));
        let expected = sort_simple_expr(vec![vec![
            Atom::Exponent("e1".into(), 1),
            Atom::Exponent("e1".into(), 1),
            Atom::Exponent("e1".into(), 1),
            Atom::Exponent("e2".into(), 1),
            Atom::Exponent("e2".into(), 1),
        ]]);

        assert_eq!(result, expected);
        Ok(())
    }
}