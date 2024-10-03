use super::types::{Annotation, ParseAnnotation, ParseExpr, ParseType, StaticAnnotation};

use crate::types::{Expr, Type};

#[derive(Debug, PartialEq)]
pub enum ParseConvertError {
    FoundError,
    MissingTypeAnnotation,
}

// turn this into our 'real' expr that does not include `Error`
// so we can typecheck it and generally try it out
pub fn to_real_expr(parse_expr: ParseExpr) -> Result<Expr<StaticAnnotation>, ParseConvertError> {
    match parse_expr {
        ParseExpr::Ann { ann, ty, expr } => Ok(Expr::EAnn {
            ann: to_real_ann(ann),
            ty: to_real_ty(ty)?,
            expr: Box::new(to_real_expr(*expr)?),
        }),
        ParseExpr::Ident { ann, var } => Ok(Expr::EIdent {
            ann: to_real_ann(ann),
            var,
        }),
        ParseExpr::Prim { ann, prim } => Ok(Expr::EPrim {
            ann: to_real_ann(ann),
            prim,
        }),
        ParseExpr::If {
            ann,
            pred_expr,
            then_expr,
            else_expr,
        } => Ok(Expr::EIf {
            ann: to_real_ann(ann),
            pred_expr: Box::new(to_real_expr(*pred_expr)?),
            then_expr: Box::new(to_real_expr(*then_expr)?),
            else_expr: Box::new(to_real_expr(*else_expr)?),
        }),
        ParseExpr::Error => Err(ParseConvertError::FoundError),
    }
}

pub fn to_real_ty(
    parse_ty: Option<ParseType>,
) -> Result<Type<StaticAnnotation>, ParseConvertError> {
    parse_ty
        .map(|ty| match ty {
            ParseType::Prim { ann, type_prim } => Type::TPrim {
                ann: to_real_ann(ann),
                type_prim,
            },
        })
        .ok_or(ParseConvertError::MissingTypeAnnotation)
}

fn to_real_ann(ann: ParseAnnotation) -> StaticAnnotation {
    Annotation {
        start: ann.start.map_extra(|_| ()),
        end: ann.end.map_extra(|_| ()),
    }
}
