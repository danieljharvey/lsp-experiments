use crate::parser::Annotation;
use crate::typecheck::get_outer_type_annotation;
use crate::types::{Prim, Type, TypePrim};
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError<Ann> {
    UnknownVariable {
        ann: Ann,
        var: String,
    },
    UnknownIntegerLiteral {
        ann: Ann,
    },
    TypeMismatch {
        expected: Type<Ann>,
        actual: Type<Ann>,
    },
    LiteralMismatch {
        ann: Ann,
        prim: Prim,
        type_prim: TypePrim,
    },
}

fn range_from_annotation(ann: &Annotation) -> Range<usize> {
    ann.start.offset..ann.end.offset
}

pub fn to_report(type_error: &TypeError<Annotation>) -> ariadne::Report<Range<usize>> {
    use ariadne::{Label, Report, ReportKind};

    match type_error {
        TypeError::UnknownIntegerLiteral { ann } => Report::build(ReportKind::Error, (), 12)
            .with_code(1)
            .with_message("Unknown integer literal")
            .with_label(Label::new(range_from_annotation(ann)).with_message(
                "This could be an Int8, Int16, Int32 or Int64. Consider adding a type annotation",
            ))
            .finish(),
        TypeError::UnknownVariable { var, ann } => Report::build(ReportKind::Error, (), 12)
            .with_code(1)
            .with_message("Unknown variable")
            .with_label(
                Label::new(range_from_annotation(ann))
                    .with_message(format!("Can't find variable {var}")),
            )
            .finish(),

        TypeError::TypeMismatch { expected, actual } => Report::build(ReportKind::Error, (), 12)
            .with_code(2)
            .with_message("Type mismatch")
            .with_label(
                Label::new(range_from_annotation(get_outer_type_annotation(actual))).with_message(
                    format!("Expected type {} but got type {}", expected, actual),
                ),
            )
            .finish(),
        TypeError::LiteralMismatch {
            ann,
            prim,
            type_prim,
        } => Report::build(ReportKind::Error, (), 12)
            .with_code(2)
            .with_message("Literal type mismatch")
            .with_label(Label::new(range_from_annotation(ann)).with_message(format!(
                "Literal value {} does not match type {}",
                prim, type_prim
            )))
            .finish(),
    }
}
