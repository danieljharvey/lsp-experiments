use crate::parser::Annotation;
use crate::types::{Prim, Type, TypePrim};
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError<Ann> {
    UnknownIntegerLiteral {
        ann: Ann,
    },
    TypeMismatch {
        ty_left: Type<Ann>,
        ty_right: Type<Ann>,
    },
    LiteralMismatch {
        ann: Ann,
        prim: Prim,
        type_prim: TypePrim,
    },
}

pub fn to_report<'a>(type_error: &'a TypeError<Annotation>) -> ariadne::Report<'a, Range<usize>> {
    use ariadne::{Label, Report, ReportKind};

    match type_error {
        TypeError::UnknownIntegerLiteral { ann: _ } => Report::build(ReportKind::Error, (), 12)
            .with_code(1)
            .with_message("Unknown integer literal")
            .with_label(Label::new(1..10).with_message(
                "This could be an Int8, Int16, Int32 or Int64. Consider adding a type annotation",
            ))
            .finish(),
        _ => todo!("to_report for other type_error arms"),
    }
}
