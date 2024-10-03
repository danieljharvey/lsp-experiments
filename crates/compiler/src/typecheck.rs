mod warning;
use crate::types::{Expr, Prim, Type, TypePrim};
pub use warning::Warning;
mod type_error;
pub use type_error::{to_report, TypeError};
mod expr_utils;
pub use expr_utils::{
    get_outer_expr_annotation, get_outer_type_annotation, set_outer_expr_annotation,
    set_outer_type_annotation,
};

fn infer_prim<Ann>(ann: Ann, prim: &Prim) -> Result<Type<Ann>, TypeError<Ann>> {
    match prim {
        Prim::Boolean(_) => Ok(Type::TPrim {
            ann,
            type_prim: TypePrim::TBoolean,
        }),
        Prim::IntLit(_) => Err(TypeError::UnknownIntegerLiteral { ann }),
    }
}

// given no information, try and work out what we have here
pub fn infer<Ann: Clone>(
    expr: &Expr<Ann>,
    warnings: &mut Vec<Warning<Ann>>,
) -> Result<Expr<Type<Ann>>, TypeError<Ann>> {
    match expr {
        Expr::EPrim { ann, prim } => {
            let ty = infer_prim(ann.clone(), prim)?;
            Ok(Expr::EPrim {
                ann: ty,
                prim: prim.clone(),
            })
        }
        Expr::EIdent { ann, var } => Err(TypeError::UnknownVariable {
            var: var.clone(),
            ann: ann.clone(),
        }),
        Expr::EIf {
            ann,
            pred_expr,
            then_expr,
            else_expr,
        } => check_if(None, ann, pred_expr, then_expr, else_expr, warnings),
        Expr::EAnn { ann, ty, expr } => {
            let typed_expr = check(ty, expr, warnings)?;
            let inferred_type = get_outer_expr_annotation(&typed_expr);
            if let Type::TPrim {
                type_prim: TypePrim::TBoolean,
                ..
            } = inferred_type
            {
                // ie, we don't need this annotation
                warnings.push(Warning::UnnecessaryAnnotation { ann: ann.clone() })
            }
            let ty = set_outer_type_annotation(inferred_type, ann);
            Ok(set_outer_expr_annotation(&typed_expr, &ty))
        }
    }
}

fn check_if<Ann: Clone>(
    maybe_ty: Option<&Type<Ann>>,
    ann: &Ann,
    pred_expr: &Expr<Ann>,
    then_expr: &Expr<Ann>,
    else_expr: &Expr<Ann>,
    warnings: &mut Vec<Warning<Ann>>,
) -> Result<Expr<Type<Ann>>, TypeError<Ann>> {
    let typed_pred_expr = check(
        &Type::TPrim {
            ann: get_outer_expr_annotation(pred_expr).clone(),
            type_prim: TypePrim::TBoolean,
        },
        pred_expr,
        warnings,
    )?;
    let typed_then_expr = match maybe_ty {
        Some(ty) => check(ty, then_expr, warnings),
        None => infer(then_expr, warnings),
    }?;
    let ty_then = get_outer_expr_annotation(&typed_then_expr);
    let typed_else_expr = check(ty_then, else_expr, warnings)?;
    let ty = set_outer_type_annotation(ty_then, ann);
    Ok(Expr::EIf {
        ann: ty,
        pred_expr: Box::new(typed_pred_expr),
        then_expr: Box::new(typed_then_expr),
        else_expr: Box::new(typed_else_expr),
    })
}

// given a type, try and work out what we have here
fn check<Ann: Clone>(
    ty: &Type<Ann>,
    expr: &Expr<Ann>,
    warnings: &mut Vec<Warning<Ann>>,
) -> Result<Expr<Type<Ann>>, TypeError<Ann>> {
    match (ty, expr) {
        (
            ty,
            Expr::EIf {
                ann,
                pred_expr,
                then_expr,
                else_expr,
            },
        ) => check_if(Some(ty), ann, pred_expr, then_expr, else_expr, warnings),
        (ty, Expr::EIdent { ann, var }) => {
            // whatever the type sig says goes
            let ty_ident = set_outer_type_annotation(ty, ann);
            Ok(Expr::EIdent {
                ann: ty_ident,
                var: var.clone(),
            })
        }
        (Type::TPrim { type_prim, .. }, Expr::EPrim { prim, ann }) => {
            check_prim(ann, type_prim, prim)?;
            Ok(Expr::EPrim {
                ann: Type::TPrim {
                    ann: ann.clone(),
                    type_prim: type_prim.clone(),
                },
                prim: prim.clone(),
            })
        }
        _ => {
            let typed_expr = infer(expr, warnings)?;
            let ty_expr = get_outer_expr_annotation(&typed_expr);
            let _resolved_ty = unify(ty, ty_expr)?;
            // once we have polymorphism, we'll need to insert `resolved_ty` into `typed_expr`
            // but for now we can just return it
            Ok(typed_expr)
        }
    }
}

fn unify<Ann: Clone>(
    expected: &Type<Ann>,
    actual: &Type<Ann>,
) -> Result<Type<Ann>, TypeError<Ann>> {
    match (expected, actual) {
        (
            Type::TPrim {
                type_prim: type_prim_left,
                ..
            },
            Type::TPrim {
                type_prim: type_prim_right,
                ..
            },
        ) => {
            if type_prim_left == type_prim_right {
                Ok(expected.clone())
            } else {
                Err(TypeError::TypeMismatch {
                    expected: expected.clone(),
                    actual: actual.clone(),
                })
            }
        }
    }
}

fn check_prim<Ann: Clone>(
    ann: &Ann,
    type_prim: &TypePrim,
    prim: &Prim,
) -> Result<(), TypeError<Ann>> {
    match (type_prim, prim) {
        (TypePrim::TBoolean, Prim::Boolean(_)) => Ok(()),
        (TypePrim::TInt8, Prim::IntLit(_)) => Ok(()),
        (TypePrim::TInt16, Prim::IntLit(_)) => Ok(()),
        (TypePrim::TInt32, Prim::IntLit(_)) => Ok(()),
        (TypePrim::TInt64, Prim::IntLit(_)) => Ok(()),

        _ => Err(TypeError::LiteralMismatch {
            ann: ann.clone(),
            type_prim: type_prim.clone(),
            prim: prim.clone(),
        }),
    }
}
