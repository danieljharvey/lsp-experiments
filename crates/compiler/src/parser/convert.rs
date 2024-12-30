use super::types::{Annotation, ParseBlock, ParseExpr, ParseFunction, ParseType};

use crate::types::{Expr, Function, Type};

#[derive(Debug, PartialEq)]
pub enum ParseConvertError {
    MissingIdent,
    FoundError,
    MissingTypeAnnotation,
}

pub fn parse_function_to_function(
    parse_function: ParseFunction,
) -> Result<Function<Annotation>, ParseConvertError> {
    Ok(Function {
        name: parse_function.name,
        arguments: parse_function
            .arguments
            .into_iter()
            .map(|(name, ty)| {
                (
                    name,
                    ty.ok_or(ParseConvertError::MissingTypeAnnotation)
                        .map(|ty| to_real_ty(ty)?),
                )
            })
            .collect(),
        return_type: parse_function.return_type.map(|ty| Some(to_real_ty(ty)?)),
        body: parse_block_to_expr(parse_function.block)?,
    })
}

pub fn parse_block_to_expr(parse_block: ParseBlock) -> Result<Expr<Annotation>, ParseConvertError> {
    let mut final_expr = to_real_expr(parse_block.final_expr)?;
    for (ann, ident, exp) in parse_block.let_bindings {
        final_expr = Expr::ELet {
            ann,
            var: ident.ok_or(ParseConvertError::MissingIdent)?,
            expr: Box::new(to_real_expr(exp)?),
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
