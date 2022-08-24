#[cfg(test)]

pub mod t {
    extern crate test;
    use std::io::Result;

    use crate::implicit_surfaces::expr::{parse_string};
    use crate::implicit_surfaces::poly::{expr_to_poly, poly_as_list};
    use crate::implicit_surfaces::simple_expr::{self, expr_to_simple_expr};
    use crate::implicit_surfaces::uni_poly::{uni_poly_derivative, self, to_int_simple_expr_vec, UniPoly, sturm_seq};

    fn to_uni_poly(s: String, v: &str, ray_values: [f64; 6]) -> UniPoly {
        uni_poly::to_uni_poly(
            &to_int_simple_expr_vec(
                poly_as_list(
                    &expr_to_poly(
                        parse_string(s), v
        ))), ray_values)
    }

    #[test]
    fn parse_to_uni_poly_derivative() -> Result<()> {
        let input = "3x^3 + 3x^2 + 5x + 1".to_string();
        let result = 
            uni_poly_derivative(
                &to_uni_poly(
                    input, "x", [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]));
        let expected = vec![
            (2, 9.0),
            (1, 6.0),
            (0, 5.0),
        ];

        assert_eq!(result, expected);
        Ok(())
    }

    #[test]
    fn parse_to_uni_poly_derivate_negative() -> Result<()> {
        let input = "- 3x^3 - 3x^2 - 5x - 1".to_string();
        let result = 
            uni_poly_derivative(
                &to_uni_poly(
                    input, "x", [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]));
        let expected = vec![
            (2, -9.0),
            (1, -6.0),
            (0, -5.0),
        ];

        assert_eq!(result, expected);
        Ok(())
    }

    #[test]
    fn parse_sturm_seq() -> Result<()> {
        let input = "x^4 + 2x^2 + 4x + 1".to_string();
        let parsed = parse_string(input.clone());
        println!("expr: \n{:?}\n", parsed);
        let se = expr_to_simple_expr(parsed.clone());
        println!("se: \n{:?}\n", se);
        let poly = expr_to_poly(parsed, "x");
        println!("poly: \n{:?}\n", poly);

        let unipoly = to_uni_poly(input, "x", [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]);
        println!("{:?}", unipoly);
        let result = sturm_seq(&unipoly, &uni_poly_derivative(&unipoly));
        let expected = vec![
            vec![(4, 1.0), (2, 2.0), (1, 4.0), (0, 1.0)],
            vec![(3, 4.0), (1, 4.0), (0, 4.0)],
            vec![(2, -1.0), (1, -3.0), (0, -1.0)],
            vec![(1, -36.0), (0, -16.0)],
            vec![(0, 1.0 + (-92.0 / 81.0))],
        ];

        assert_eq!(result, expected);
        Ok(())
    }
}