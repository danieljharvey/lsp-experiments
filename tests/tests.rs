use frame::parser::{bool, int, mk_if, parse_expr};
use frame::typecheck::TypeError;
use frame::types::{Expr, Type, TypePrim};

fn void_expr<Ann>(expr: Expr<Ann>) -> Expr<()> {
    match expr {
        Expr::EPrim { prim, .. } => Expr::EPrim { ann: (), prim },
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
    ];

    for (input, expect) in tests {
        let result = parse_expr(input.into());
        dbg!(&result);
        let voided_result = result.map(|(_, expr)| void_expr(expr));
        assert_eq!(voided_result, Ok(expect))
    }
}

#[test]
fn test_typecheck_success() {
    let tests = vec![
        ("True", "Boolean"),
        ("False", "Boolean"),
        ("if True then False else True", "Boolean"),
        ("if True then (1: Int64) else 2", "Int64"),
    ];

    for (input, expected) in tests {
        let (_, input_expr) = frame::parser::parse_expr(input.into()).expect("parsing expr");
        let (_, expected_type) = frame::parser::parse_type(expected.into()).expect("parsing type");

        let typed_expr =
            frame::typecheck::infer(&void_expr(input_expr)).expect("should have succeeded");
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
                ty_left: Type::TPrim {
                    ann: (),
                    type_prim: TypePrim::TBoolean,
                },
                ty_right: Type::TPrim {
                    ann: (),
                    type_prim: TypePrim::TInt64,
                },
            },
        ),
        (
            "if True then True else (1: Int64)",
            TypeError::TypeMismatch {
                ty_left: Type::TPrim {
                    ann: (),
                    type_prim: TypePrim::TBoolean,
                },
                ty_right: Type::TPrim {
                    ann: (),
                    type_prim: TypePrim::TInt64,
                },
            },
        ),
    ];

    for (input, expected_type_error) in tests {
        let (_, input_expr) = frame::parser::parse_expr(input.into()).expect("parsing expr");

        let result = frame::typecheck::infer(&void_expr(input_expr));

        assert_eq!(result, Err(expected_type_error));
    }
}
