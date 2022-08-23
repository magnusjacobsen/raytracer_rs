#[cfg(test)]

pub mod t {
    extern crate test;
    use std::io::Result;

    use crate::implicit_surfaces::simple_expr::{se_num};
    use crate::implicit_surfaces::expr::{parse_string};
    use crate::implicit_surfaces::poly::{expr_to_poly, poly_as_list, poly_derivative, to_string_poly};

    #[test]
    fn string_to_derivative_list() -> Result<()> {
        let input = "3x^3 + 3x^2 + 5x + 1".to_string();
        let result = poly_as_list(&poly_derivative(expr_to_poly(parse_string(input), "x")));
        let expected = vec![
            (0, se_num(5.0)),
            (1, se_num(6.0)),
            (2, se_num(9.0))
        ];

        assert_eq!(result, expected);
        Ok(())
    }

    #[test]
    fn polynomial_derivative() -> Result<()> {
        let input = "4*x^2 + z*x^3 + 4*x^5*z^2".to_string();
        let poly = expr_to_poly(parse_string(input), "z");
        let result = to_string_poly("z", &poly);
        let expected = "(4*x^2)+z(x^3)+z^2(4*x^5)".to_string();
        let result_derivative = to_string_poly("z", &poly_derivative(poly));
        let expected_derivative = "(x^3)+z(8*x^5)".to_string();

        assert_eq!(result, expected);
        assert_eq!(result_derivative, expected_derivative);
        Ok(())
    }
}