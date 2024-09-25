use std::fmt::{Display, Error, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum Prim {
    Boolean(bool),
    IntLit(i64),
}

impl Display for &Prim {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Prim::Boolean(true) => write!(f, "True"),
            Prim::Boolean(false) => write!(f, "False"),
            Prim::IntLit(i) => i.fmt(f),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypePrim {
    TBoolean,
    TInt8,
    TInt16,
    TInt32,
    TInt64,
}

impl Display for &TypePrim {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            TypePrim::TBoolean => write!(f, "Boolean"),
            TypePrim::TInt8 => write!(f, "Int8"),
            TypePrim::TInt16 => write!(f, "Int16"),
            TypePrim::TInt32 => write!(f, "Int32"),
            TypePrim::TInt64 => write!(f, "Int64"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type<Ann> {
    TPrim { ann: Ann, type_prim: TypePrim },
}

impl<Ann> Display for &Type<Ann> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Type::TPrim { type_prim, .. } => type_prim.fmt(f),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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

impl<Ann> Display for Expr<Ann> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            Expr::EPrim { prim, .. } => prim.fmt(f),
            Expr::EIf {
                pred_expr,
                then_expr,
                else_expr,
                ..
            } => write!(
                f,
                "if {} then {} else {}",
                &pred_expr, &then_expr, &else_expr
            ),
            Expr::EAnn { ty, expr, .. } => write!(f, "({}: {})", &expr, ty),
        }
    }
}
