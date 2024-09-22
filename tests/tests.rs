use frame::parser::{bool, int, mk_if, parse_expr};
use frame::types::{Expr, Type, TypePrim};

fn get_outer_expr_annotation<Ann>(expr: Expr<Ann>) -> Ann {
    match expr {
        Expr::EPrim { ann, .. } => ann,
        Expr::EIf { ann, .. } => ann,
        Expr::EAnn { ann, .. } => ann,
    }
}

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
fn test_typecheck() {
    let tests = vec![
        ("True", "Boolean"),
        ("False", "Boolean"),
        ("if True then False else True", "Boolean"),
    ];

    for (input, expected) in tests {
        let (_, input_expr) = frame::parser::parse_expr(input.into()).expect("parsing expr");
        let (_, expected_type) = frame::parser::parse_type(expected.into()).expect("parsing type");

        let result = frame::typecheck::infer(void_expr(input_expr));

        assert_eq!(
            result.map(get_outer_expr_annotation),
            Ok(void_type(expected_type))
        );
    }
}
