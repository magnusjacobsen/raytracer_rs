#[cfg(test)]

pub mod t {
    extern crate test;
    use std::io::Result;

    use crate::implicit_surfaces::solving::{newton_raphson, subst_with_ray};
    use crate::implicit_surfaces::expr::{parse_string};
    use crate::implicit_surfaces::poly::{expr_to_poly, poly_as_list, to_string_poly};
    use crate::implicit_surfaces::uni_poly::{to_uni_poly, to_int_simple_expr_vec, uni_poly_derivative};

    #[test]
    fn simple_newton_raphson() -> Result<()> {
        let input = to_uni_poly(
            &to_int_simple_expr_vec(
                poly_as_list(
                    &expr_to_poly(
                        parse_string("3x^2 - 3".into()), "x"
                    )
                )
            ), [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]);
        let result = newton_raphson(&input, &uni_poly_derivative(&input), 10, 0.1);
        let expected = Some(1.0);
        
        assert_eq!(result, expected);
        Ok(())
    }

    #[test]
    fn simple_newton_raphson_fail() -> Result<()> {
        let input = to_uni_poly(
            &to_int_simple_expr_vec(
                poly_as_list(
                    &expr_to_poly(
                        parse_string("3x^2".into()), "x"
                    )
                )
            ), [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]);
        let result = newton_raphson(&input, &uni_poly_derivative(&input), 10, 0.1);
        let expected = None;
        
        assert_eq!(result, expected);
        Ok(())
    }

    #[test]
    fn expr_to_poly_simple_equation() -> Result<()> {
        let input = "x^2 + y^2 + z^2 - r^2".to_string();
        let input_radical = "(x^2 + y^2 + z^2)_2 - r".to_string();
        let result = to_string_poly("t", &expr_to_poly(subst_with_ray(&parse_string(input)), "t"));
        let result_radical = to_string_poly("t", &expr_to_poly(subst_with_ray(&parse_string(input_radical)), "t"));
        let expected = "(ox^2+oz^2+-1*r^2+oy^2)+t(2*dy*oy+2*dz*oz+2*dx*ox)+t^2(dx^2+dz^2+dy^2)".to_string();

        assert_eq!(result, expected);
        assert_eq!(result_radical, expected);
        Ok(())
    }
}