#[derive(Debug, PartialEq)]
pub enum Prim {
    Boolean(bool),
    IntLit(i64),
}

#[derive(Debug, PartialEq)]
pub enum TypePrim {
    TBoolean,
    TInt8,
    TInt16,
    TInt32,
    TInt64,
}

#[derive(Debug, PartialEq)]
pub enum Type<Ann> {
    TPrim { ann: Ann, type_prim: TypePrim },
}

#[derive(Debug, PartialEq)]
pub enum Expr<Ann> {
    EPrim {
        ann: Ann,
        prim: Prim,
    },
    EIf {
        ann: Ann,
        pred_expr: Box<Expr<Ann>>,
        then_expr: Box<Expr<Ann>>,
        else_expr: Box<Expr<Ann>>,
    },
    EAnn {
        ann: Ann,
        ty: Type<Ann>,
        expr: Box<Expr<Ann>>,
    },
}
