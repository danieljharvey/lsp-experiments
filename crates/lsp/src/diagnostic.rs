use crate::CompileError;
use frame::{
    parser::Annotation,
    typecheck::{get_outer_type_annotation, TypeError},
    types::{Expr, Type},
};
use tower_lsp::lsp_types::*;

pub enum Diag<'a> {
    Error {
        message: String,
        annotation: Annotation<'a>,
    },
}

pub fn to_diagnostic(diag: &Diag) -> Diagnostic {
    match diag {
        Diag::Error {
            message,
            annotation,
        } => Diagnostic {
            code: None,
            code_description: None,
            data: None,
            message: message.to_string(),
            range: crate::range_from_annotation(&annotation),
            related_information: None,
            severity: Some(DiagnosticSeverity::ERROR),
            source: None,
            tags: None,
        },
    }
}

// get all diagnostics given compiler output
// for now, just errors, but we can also emit warnings on success
pub fn get_diagnostics<'a>(
    result: Result<Expr<Type<Annotation<'a>>>, CompileError<'a>>,
) -> Vec<Diag<'a>> {
    match result {
        Ok(_) => vec![],
        Err(CompileError::ParseError) => vec![],
        Err(CompileError::TypeError(type_error)) => match type_error {
            TypeError::UnknownIntegerLiteral { ann } => {
                vec![Diag::Error {
                    message: "Unknown integer literal".to_string(),
                    annotation: ann,
                }]
            }
            TypeError::LiteralMismatch {
                ann,
                prim,
                type_prim,
            } => {
                vec![Diag::Error {
                    message: format!(
                        "Literal mismatch. Expected value {} to have type {}",
                        &prim, &type_prim,
                    ),
                    annotation: ann,
                }]
            }
            TypeError::TypeMismatch { expected, actual } => {
                let annotation = get_outer_type_annotation(&actual);
                vec![Diag::Error {
                    message: format!("Expected type {} but got type {}", &expected, &actual),
                    annotation: annotation.clone(),
                }]
            }
        },
    }
}
