#[cfg(test)]

pub mod t {
    extern crate test;
    use std::io::Result;

    use crate::implicit_surfaces::simple_expr::{Atom, combine, self};
    use crate::implicit_surfaces::expr::{self};

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
            Atom::Exponent("b".into(), 1),
            Atom::Exponent("a".into(), 1)
        ]];

        assert_eq!(result, expected);
        Ok(())
    }
}