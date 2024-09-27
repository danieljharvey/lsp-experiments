use crate::CompileError;
use frame::{
    parser::Annotation,
    typecheck::{get_outer_type_annotation, TypeError, Warning},
    types::{Expr, Type},
};
use tower_lsp::lsp_types::*;

pub enum Diag<'a> {
    Error {
        message: String,
        annotation: Annotation<'a>,
    },
    Warning {
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
            range: crate::hover::range_from_annotation(annotation),
            related_information: None,
            severity: Some(DiagnosticSeverity::ERROR),
            source: None,
            tags: None,
        },
        Diag::Warning {
            message,
            annotation,
        } => Diagnostic {
            code: None,
            code_description: None,
            data: None,
            message: message.to_string(),
            range: crate::hover::range_from_annotation(annotation),
            related_information: None,
            severity: Some(DiagnosticSeverity::WARNING),
            source: None,
            tags: None,
        },
    }
}

// get all diagnostics given compiler output
// for now, just errors, but we can also emit warnings on success
pub fn get_diagnostics<'a>(
    (result, warnings): (
        Result<Expr<Type<Annotation<'a>>>, CompileError<'a>>,
        Vec<Warning<Annotation<'a>>>,
    ),
) -> Vec<Diag<'a>> {
    let mut error_diagostics = match result {
        Ok(_) => vec![],
        Err(CompileError::ParseError) => vec![],
        Err(CompileError::TypeError(type_error)) => match *type_error {
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
    };
    let warning_diagnostics: Vec<_> = warnings
        .iter()
        .map(|warning| match warning {
            Warning::UnnecessaryAnnotation { ann } => Diag::Warning {
                message: "This annotation is not required".to_string(),
                annotation: ann.clone(),
            },
        })
        .collect();

    error_diagostics.extend(warning_diagnostics);

    error_diagostics
}
