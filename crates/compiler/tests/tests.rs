use frame::parser::constructors::{bool, int, mk_if};
use frame::typecheck::{Env, TypeError};
use frame::types::{Expr, Prim, Type, TypePrim};

fn void_expr<Ann>(expr: Expr<Ann>) -> Expr<()> {
    match expr {
        Expr::EPrim { prim, .. } => Expr::EPrim { ann: (), prim },
        Expr::EIdent { var, .. } => Expr::EIdent { ann: (), var },
        Expr::ELet {
            var, expr, rest, ..
        } => Expr::ELet {
            ann: (),
            var,
            expr: Box::new(void_expr(*expr)),
            rest: Box::new(void_expr(*rest)),
        },
        Expr::EIf {
            pred_expr,
            then_expr,
            else_expr,
            ..
        } => Expr::EIf {
            ann: (),
            pred_expr: Box::new(void_expr(*pred_expr)),
            then_expr: Box::new(void_expr(*then_expr)),
            else_expr: Box::new(void_expr(*else_expr)),
        },
        Expr::EAnn { ty, expr, .. } => Expr::EAnn {
            ann: (),
            ty: void_type(ty),
            expr: Box::new(void_expr(*expr)),
        },
    }
}

fn void_type<Ann>(ty: Type<Ann>) -> Type<()> {
    match ty {
        Type::TPrim { type_prim, .. } => Type::TPrim { ann: (), type_prim },
    }
}
fn void_type_error<Ann>(type_error: TypeError<Ann>) -> TypeError<()> {
    match type_error {
        TypeError::TypeMismatch { expected, actual } => TypeError::TypeMismatch {
            expected: void_type(expected),
            actual: void_type(actual),
        },
        TypeError::UnknownVariable { var, .. } => TypeError::UnknownVariable { ann: (), var },
        TypeError::LiteralMismatch {
            prim, type_prim, ..
        } => TypeError::LiteralMismatch {
            ann: (),
            prim,
            type_prim,
        },
        TypeError::UnknownIntegerLiteral { .. } => TypeError::UnknownIntegerLiteral { ann: () },
    }
}

#[test]
fn test_parse() {
    let tests = vec![
        (" 1", int((), 1)),
        ("1", int((), 1)),
        ("11", int((), 11)),
        ("    11dog", int((), 11)),
        (" True", bool((), true)),
        ("False", bool((), false)),
        ("    True100", bool((), true)),
        (
            "dog",
            Expr::EIdent {
                ann: (),
                var: "dog".to_string(),
            },
        ),
        (
            " (   dog: Int64)",
            Expr::EAnn {
                ann: (),
                ty: Type::TPrim {
                    ann: (),
                    type_prim: TypePrim::TInt64,
                },
                expr: Box::new(Expr::EIdent {
                    ann: (),
                    var: "dog".to_string(),
                }),
            },
        ),
        (
            "if 1 then False else True",
            mk_if((), int((), 1), bool((), false), bool((), true)),
        ),
        (
            "if True then 1 else 2",
            mk_if((), bool((), true), int((), 1), int((), 2)),
        ),
        (
            "(1: Int64)",
            Expr::EAnn {
                ann: (),
                ty: Type::TPrim {
                    ann: (),
                    type_prim: TypePrim::TInt64,
                },
                expr: Box::new(int((), 1)),
            },
        ),
        (
            "let a = True; a",
            Expr::ELet {
                ann: (),
                var: "a".to_string(),
                expr: Box::new(bool((), true)),
                rest: Box::new(Expr::EIdent {
                    ann: (),
                    var: "a".to_string(),
                }),
            },
        ),
    ];

    for (input, expect) in tests {
        let (parse_result, _parse_errors) = frame::parser::parse(input);
        let result = frame::parser::parse_block_to_expr(parse_result);
        let voided_result = result.map(void_expr);
        assert_eq!(voided_result, Ok(expect))
    }
}

#[test]
fn test_parse_errors() {
    let tests = vec![
        "let False = True; a",
        "if True then 1 else",
        "dog 1",
        "let a = !!; a",
    ];

    for input in tests {
        let (parse_result, parse_errors) = frame::parser::parse(input);
        insta::assert_debug_snapshot!(parse_result);
        insta::assert_debug_snapshot!(parse_errors);
    }
}

#[test]
fn test_typecheck_success() {
    let tests = vec![
        ("True", "Boolean"),
        ("False", "Boolean"),
        ("if True then False else True", "Boolean"),
        ("if True then (1: Int64) else 2", "Int64"),
        ("(if True then 1 else 2 : Int64)", "Int64"),
        ("let a = True; a", "Boolean"),
    ];

    for (input, expected) in tests {
        let (parse_result, _) = frame::parser::parse(input);
        let input_expr = frame::parser::parse_block_to_expr(parse_result).expect("parsing expr");

        let (expected_parse_type, _) = frame::parser::parse_type(expected);
        let expected_type =
            frame::parser::to_real_ty(Some(expected_parse_type)).expect("parsing type");

        let typed_expr = frame::typecheck::infer(&void_expr(input_expr), &mut Env::new())
            .expect("should have succeeded");
        let ty = frame::typecheck::get_outer_expr_annotation(&typed_expr);

        assert_eq!(ty, &void_type(expected_type));
    }
}

#[test]
fn test_typecheck_failure() {
    let tests = vec![
        (
            "if True then 1 else 2",
            TypeError::UnknownIntegerLiteral { ann: () },
        ),
        (
            "if (1: Int64) then True else False",
            TypeError::TypeMismatch {
                expected: Type::TPrim {
                    ann: (),
                    type_prim: TypePrim::TBoolean,
                },
                actual: Type::TPrim {
                    ann: (),
                    type_prim: TypePrim::TInt64,
                },
            },
        ),
        (
            "if True then True else (1: Int64)",
            TypeError::TypeMismatch {
                expected: Type::TPrim {
                    ann: (),
                    type_prim: TypePrim::TBoolean,
                },
                actual: Type::TPrim {
                    ann: (),
                    type_prim: TypePrim::TInt64,
                },
            },
        ),
        (
            "(1: Boolean)",
            TypeError::LiteralMismatch {
                ann: (),
                type_prim: TypePrim::TBoolean,
                prim: Prim::IntLit(1),
            },
        ),
        (
            "let a = (1: Int64); if a then False else True",
            TypeError::TypeMismatch {
                expected: Type::TPrim {
                    ann: (),
                    type_prim: TypePrim::TBoolean,
                },
                actual: Type::TPrim {
                    ann: (),
                    type_prim: TypePrim::TInt64,
                },
            },
        ),
        (
            "let a = True; (a: Int64)",
            TypeError::TypeMismatch {
                expected: Type::TPrim {
                    ann: (),
                    type_prim: TypePrim::TInt64,
                },
                actual: Type::TPrim {
                    ann: (),
                    type_prim: TypePrim::TBoolean,
                },
            },
        ),
    ];

    for (input, expected_type_error) in tests {
        let (parse_result, _) = frame::parser::parse(input);
        let input_expr = frame::parser::parse_block_to_expr(parse_result).expect("parsing expr");

        let result = frame::typecheck::infer(&input_expr, &mut Env::new());

        let type_error = match result {
            Err(e) => e,
            Ok(_) => panic!("Input '{input}' succeeded and should have failed"),
        };

        assert_eq!(void_type_error(type_error.clone()), expected_type_error);

        let mut buf = std::io::BufWriter::new(Vec::new());

        let report = frame::typecheck::to_report(&type_error);

        report
            .write(ariadne::Source::from(input), &mut buf)
            .unwrap();

        let bytes = buf.into_inner().unwrap();
        let string = String::from_utf8(bytes).unwrap();

        println!("{string}");
        insta::assert_snapshot!(string);
    }
}
