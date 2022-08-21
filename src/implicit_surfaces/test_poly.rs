#[cfg(test)]

pub mod t {
    extern crate test;
    use std::io::Result;

    use crate::implicit_surfaces::simple_expr::{se_num};
    use crate::implicit_surfaces::expr::{parse_string};
    use crate::implicit_surfaces::poly::{expr_to_poly, poly_as_list, poly_derivative};

    #[test]
    fn string_to_derivative_list() -> Result<()> {
        let input = "3x^3 + 3x^2 + 5x + 1".to_string();
        let result = poly_as_list(poly_derivative(expr_to_poly(parse_string(input), "x".to_string())));
        let expected = vec![
            (0, se_num(5.0)),
            (1, se_num(6.0)),
            (2, se_num(9.0))
        ];

        assert_eq!(result, expected);
        Ok(())
    }
}