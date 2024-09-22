use crate::types::{Expr, Prim, Type, TypePrim};

#[derive(Debug, PartialEq)]
pub enum TypeError<Ann> {
    What { ann: Ann },
}

fn infer_prim<Ann>(ann: Ann, prim: &Prim) -> Result<Type<Ann>, TypeError<Ann>> {
    match prim {
        Prim::Boolean(_) => Ok(Type::TPrim {
            ann,
            type_prim: TypePrim::TBoolean,
        }),
        _ => todo!("infer prim"),
    }
}

// given no information, try and work out what we have here
pub fn infer<Ann>(expr: Expr<Ann>) -> Result<Expr<Type<Ann>>, TypeError<Ann>> {
    match expr {
        Expr::EPrim { ann, prim } => {
            let ty = infer_prim(ann, &prim)?;
            Ok(Expr::EPrim { ann: ty, prim })
        }
        Expr::EIf {
            ann,
            pred_expr,
            then_expr,
            else_expr,
        } => {
            let typed_pred_expr = check(
                TPrim {
                    ann,
                    type_pred: TBoolean,
                },
                pred_expr,
            )?;
        }
        _ => todo!("infer other types"),
    }
}

// given a type, try and work out what we have here
fn check<Ann>(ty: Type<ann>, expr: Expr<ann>) -> Result<Expr<Type<Ann>>, TypeError<Ann>> {
    match (ty, expr) {
        (Type::TPrim { type_prim, .. }, Expr::EPrim { prim, .. }) => {
            check_prim(ty_prim, prim)?;
            todo!("sdfds")
        }
    }
}

fn check_prim(ty_prim: TypePrim, prim: Prim) -> Result<(), TypeError<Ann>> {
    match (ty_prim, prim) {
        (TypePrim::TBoolean, Prim::Boolean(_)) => Ok(()),
        _ => todo!("error"),
    }
}
