use crate::types::{Expr, Type};

pub fn set_outer_type_annotation<Ann: Clone>(ty: &Type<Ann>, new_ann: &Ann) -> Type<Ann> {
    match ty {
        Type::TPrim { type_prim, .. } => Type::TPrim {
            ann: new_ann.clone(),
            type_prim: type_prim.clone(),
        },
    }
}

pub fn get_outer_type_annotation<Ann>(ty: &Type<Ann>) -> &Ann {
    match ty {
        Type::TPrim { ann, .. } => ann,
    }
}

pub fn get_outer_expr_annotation<Ann>(expr: &Expr<Ann>) -> &Ann {
    match expr {
        Expr::EPrim { ann, .. } => ann,
        Expr::EIf { ann, .. } => ann,
        Expr::EAnn { ann, .. } => ann,
    }
}

pub fn set_outer_expr_annotation<Ann: Clone>(expr: &Expr<Ann>, new_ann: &Ann) -> Expr<Ann> {
    match expr {
        Expr::EPrim { prim, .. } => Expr::EPrim {
            ann: new_ann.clone(),
            prim: prim.clone(),
        },
        Expr::EIf {
            pred_expr,
            then_expr,
            else_expr,
            ..
        } => Expr::EIf {
            ann: new_ann.clone(),
            pred_expr: pred_expr.clone(),
            then_expr: then_expr.clone(),
            else_expr: else_expr.clone(),
        },
        Expr::EAnn { ty, expr, .. } => Expr::EAnn {
            ann: new_ann.clone(),
            expr: expr.clone(),
            ty: ty.clone(),
        },
    }
}
