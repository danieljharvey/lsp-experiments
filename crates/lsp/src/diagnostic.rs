use super::CompileResult;
use crate::CompileError;
use frame::{
    parser::StaticAnnotation,
    typecheck::{get_outer_type_annotation, TypeError, Warning},
};
use tower_lsp::lsp_types::*;

pub enum Diag<'a> {
    ParseError {
        message: String,
        range: std::ops::Range<usize>,
    },
    Error {
        message: String,
        annotation: StaticAnnotation<'a>,
    },
    Warning {
        message: String,
        annotation: StaticAnnotation<'a>,
    },
}

pub fn to_diagnostic(diag: &Diag) -> Diagnostic {
    match diag {
        Diag::ParseError { message, range } => Diagnostic {
            code: None,
            code_description: None,
            data: None,
            message: message.to_string(),
            range: Range {
                start: Position {
                    character: u32::try_from(range.start).unwrap(),
                    line: 0,
                },
                end: Position {
                    character: u32::try_from(range.end).unwrap(),
                    line: 0,
                },
            },
            related_information: None,
            severity: Some(DiagnosticSeverity::ERROR),
            source: None,
            tags: None,
        },
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
            .map(|frame::parser::ParseError { range, message }| Diag::ParseError { message, range })
            .collect(),

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
