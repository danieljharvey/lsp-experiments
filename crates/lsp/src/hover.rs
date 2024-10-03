use frame::{
    parser::StaticAnnotation,
    typecheck::get_outer_type_annotation,
    types::{Expr, Type},
};
use tower_lsp::lsp_types::*;

pub fn hover_from_expr(inner_expr: &Expr<Type<StaticAnnotation>>) -> Hover {
    Hover {
        contents: HoverContents::Scalar(MarkedString::String(to_popup_message(inner_expr))),
        range: Some(range_from_annotation(get_outer_type_annotation(
            frame::typecheck::get_outer_expr_annotation(inner_expr),
        ))),
    }
}

pub fn range_from_annotation(ann: &StaticAnnotation) -> Range {
    let start_col_raw: u32 = ann.start.get_utf8_column().try_into().unwrap();
    let end_col_raw: u32 = ann.end.get_utf8_column().try_into().unwrap();

    Range {
        start: Position {
            character: start_col_raw - 1,
            line: ann.start.location_line() - 1,
        },
        end: Position {
            character: end_col_raw - 1,
            line: ann.end.location_line() - 1,
        },
    }
}

// do line/char sit within this annotation?
// this is so fucked
pub fn annotation_matches(ann: &StaticAnnotation, cursor_line: u32, cursor_col: u32) -> bool {
    let start_col_raw: u32 = ann.start.get_utf8_column().try_into().unwrap();
    let start_col: u32 = start_col_raw - 1;
    let start_line: u32 = ann.start.location_line() - 1;

    let end_col_raw: u32 = ann.end.get_utf8_column().try_into().unwrap();
    let end_col: u32 = end_col_raw - 1;
    let end_line: u32 = ann.end.location_line() - 1;

    println!(
        "start: {} {}, end: {} {}, cursor: {} {}",
        start_line, start_col, end_line, end_col, cursor_line, cursor_col
    );

    let after_start =
        (cursor_line == start_line && cursor_col >= start_col) || cursor_line > start_line;

    let before_end = (cursor_line == end_line && cursor_col < end_col) || cursor_line < end_line;

    after_start && before_end
}

#[test]
fn test_matches() {
    let ref_cell = std::cell::RefCell::new(vec![]);
    let (parse_expr, _) = frame::parser::parse(&ref_cell, "if True then (1: Int64)\nelse 2");
    let expr = frame::parser::to_real_expr(parse_expr).expect("parsing expr");

    let typed_expr = frame::typecheck::infer(&expr, &mut vec![]).unwrap();

    let tests = vec![
        (0, 2, None),
        (0, 29, None),
        (0, 13, Some("1: Int64".into())),
        (0, 18, Some("1: Int64".into())),
        (1, 5, Some("2: Int64".into())),
        (0, 3, Some("True: Boolean".into())),
    ];

    for (x, y, result) in tests {
        assert_eq!(
            find_most_specific_type(&typed_expr, x, y).map(to_popup_message),
            result
        );
    }
}

pub fn to_popup_message<Ann>(expr: &Expr<Type<Ann>>) -> String {
    let ty = frame::typecheck::get_outer_expr_annotation(expr);
    format!("{}: {}", expr, ty)
}

pub fn find_most_specific_type<'a>(
    expr: &'a Expr<Type<StaticAnnotation>>,
    line: u32,
    character: u32,
) -> Option<&'a Expr<Type<StaticAnnotation<'a>>>> {
    match expr {
        Expr::EPrim { ann, .. } => {
            if annotation_matches(get_outer_type_annotation(ann), line, character) {
                Some(expr)
            } else {
                None
            }
        }
        Expr::EAnn { ann, expr, .. } => {
            if annotation_matches(get_outer_type_annotation(ann), line, character) {
                // the whole thing matches
                // maybe we can do better though
                find_most_specific_type(expr, line, character).or(Some(expr))
            } else {
                None
            }
        }
        Expr::EIdent { ann, .. } => {
            if annotation_matches(get_outer_type_annotation(ann), line, character) {
                Some(expr)
            } else {
                None
            }
        }
        Expr::EIf {
            ann,
            pred_expr,
            then_expr,
            else_expr,
        } => {
            if annotation_matches(get_outer_type_annotation(ann), line, character) {
                find_most_specific_type(pred_expr, line, character)
                    .or(find_most_specific_type(then_expr, line, character))
                    .or(find_most_specific_type(else_expr, line, character))
            } else {
                None
            }
        }
    }
}
