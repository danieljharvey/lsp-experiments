use super::types::ParseError;
use std::ops::Range;

pub fn to_report(parse_error: &ParseError) -> ariadne::Report<Range<usize>> {
    use ariadne::{Label, Report, ReportKind};

    Report::build(ReportKind::Error, (), 12)
        .with_code(1)
        .with_message("Parse error")
        .with_label(
            Label::new(parse_error.ann.start.offset..parse_error.ann.end.offset)
                .with_message(parse_error.message.clone()),
        )
        .finish()
}
