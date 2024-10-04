use super::CompileResult;
use crate::CompileError;
use frame::{
    parser::Annotation,
    typecheck::{get_outer_type_annotation, TypeError, Warning},
};
use tower_lsp::lsp_types::*;

pub enum Diag {
    Error {
        message: String,
        annotation: Annotation,
    },
    Warning {
        message: String,
        annotation: Annotation,
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
pub fn get_diagnostics(
    CompileResult {
        expr_result: result,
        type_warnings: warnings,
    }: CompileResult,
) -> Vec<Diag> {
    let mut error_diagostics = match result {
        Ok(_) => vec![],
        Err(CompileError::ParseError(warnings)) => warnings
            .into_iter()
            .map(|frame::parser::ParseError { ann, message }| Diag::Error {
                message,
                annotation: ann,
            })
            .collect(),

        Err(CompileError::TypeError(type_error)) => match *type_error {
            TypeError::UnknownIntegerLiteral { ann } => {
                vec![Diag::Error {
                    message: "Unknown integer literal".to_string(),
                    annotation: ann,
                }]
            }
            TypeError::UnknownVariable { var, ann } => {
                vec![Diag::Error {
                    message: format!("Unknown variable {var}"),
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
