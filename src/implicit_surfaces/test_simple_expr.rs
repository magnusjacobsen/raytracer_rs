#[cfg(test)]

pub mod expr_parse_tests {
    extern crate test;
    use std::io::Result;

    use crate::implicit_surfaces::simple_expr::{Atom, self};

    #[test]
    fn combine_t1() -> Result<()> {
        /*
            combine(xs = x^2 + z, ys = y + 3)
            result = (x^2 * y) + (x^2 * 3) + (z * y) + (z * 3)
        */
        let xs = vec![vec![Atom::Exponent("x".into(), 2)], vec![Atom::Exponent("z".into(), 1)]];
        let ys = vec![vec![Atom::Exponent("y".into(), 1)], vec![Atom::Num(3.0)]];

        let expected = vec![vec![Atom::Exponent("x".into(), 2), Atom::Exponent("y".into(), 1)], vec![Atom::Exponent("x".into(), 2), Atom::Num(3.0)], vec![Atom::Exponent("z".into(), 1), Atom::Exponent("y".into(), 1)], vec![Atom::Exponent("z".into(), 1), Atom::Num(3.0)]];
        let result = simple_expr::combine(xs, ys);

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
        let result = simple_expr::combine(xs, ys);

        assert_eq!(result, expected);
        
        Ok(())
    }
}