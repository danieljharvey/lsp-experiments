use super::types::{Annotation, ParseBlock, ParseExpr, ParseType};

use crate::types::{Expr, Type};

#[derive(Debug, PartialEq)]
pub enum ParseConvertError {
    MissingIdent,
    FoundError,
    MissingTypeAnnotation,
}

pub fn parse_block_to_expr(parse_block: ParseBlock) -> Result<Expr<Annotation>, ParseConvertError> {
    let mut final_expr = to_real_expr(parse_block.final_expr)?;
    for (ann, ident, exp) in parse_block.let_bindings {
        let inner_expr = exp.ok_or(ParseConvertError::FoundError)?;
        final_expr = Expr::ELet {
            ann,
            var: ident.ok_or(ParseConvertError::MissingIdent)?,
            expr: Box::new(to_real_expr(inner_expr)?),
            rest: Box::new(final_expr.clone()),
        };
    }
    Ok(final_expr)
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
