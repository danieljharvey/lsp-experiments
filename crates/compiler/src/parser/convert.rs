use super::types::{Annotation, ParseExpr, ParseType};

use crate::types::{Expr, Type};

#[derive(Debug, PartialEq)]
pub enum ParseConvertError {
    MissingIdent,
    FoundError,

    MissingTypeAnnotation,
}

// turn this into our 'real' expr that does not include `Error`
// so we can typecheck it and generally try it out
pub fn to_real_expr(parse_expr: ParseExpr) -> Result<Expr<Annotation>, ParseConvertError> {
    match parse_expr {
        ParseExpr::Ann { ann, ty, expr } => Ok(Expr::EAnn {
            ann,
            ty: to_real_ty(ty)?,
            expr: Box::new(to_real_expr(*expr)?),
        }),
        ParseExpr::Ident { ann, var } => Ok(Expr::EIdent { ann, var }),
        ParseExpr::Prim { ann, prim } => Ok(Expr::EPrim { ann, prim }),
        ParseExpr::Let {
            ann,
            var,
            expr,
            rest,
        } => Ok(Expr::ELet {
            ann,
            var: var.ok_or(ParseConvertError::MissingIdent)?,
            expr: Box::new(to_real_expr(*expr)?),
            rest: Box::new(to_real_expr(*rest)?),
        }),
        ParseExpr::If {
            ann,
            pred_expr,
            then_expr,
            else_expr,
        } => Ok(Expr::EIf {
            ann,
            pred_expr: Box::new(to_real_expr(*pred_expr)?),
            then_expr: Box::new(to_real_expr(*then_expr)?),
            else_expr: Box::new(to_real_expr(*else_expr)?),
        }),
        ParseExpr::Error => Err(ParseConvertError::FoundError),
    }
}

pub fn to_real_ty(parse_ty: Option<ParseType>) -> Result<Type<Annotation>, ParseConvertError> {
    parse_ty
        .map(|ty| match ty {
            ParseType::Prim { ann, type_prim } => Type::TPrim { ann, type_prim },
        })
        .ok_or(ParseConvertError::MissingTypeAnnotation)
}
