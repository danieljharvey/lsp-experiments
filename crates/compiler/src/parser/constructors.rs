use crate::types::{Expr, Prim};

// construct int
pub fn int<Ann>(ann: Ann, int_val: i64) -> Expr<Ann> {
    Expr::EPrim {
        ann,
        prim: Prim::IntLit(int_val),
    }
}

// construct bool
pub fn bool<Ann>(ann: Ann, bool_val: bool) -> Expr<Ann> {
    Expr::EPrim {
        ann,
        prim: Prim::Boolean(bool_val),
    }
}

// construct if
pub fn mk_if<Ann>(
    ann: Ann,
    pred_expr: Expr<Ann>,
    then_expr: Expr<Ann>,
    else_expr: Expr<Ann>,
) -> Expr<Ann> {
    Expr::EIf {
        ann,
        pred_expr: Box::new(pred_expr),
        then_expr: Box::new(then_expr),
        else_expr: Box::new(else_expr),
    }
}
