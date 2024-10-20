use std::cell::RefCell;

use crate::types::{Prim, TypePrim};

pub type IResult<'a, T> = nom::IResult<LocatedSpan<'a>, T>;

pub type LocatedSpan<'a> = nom_locate::LocatedSpan<&'a str, State<'a>>;

#[derive(Debug, Clone, PartialEq)]
pub struct Position {
    pub offset: usize,
    pub row: u32,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Annotation {
    pub start: Position,
    pub end: Position,
}

pub trait ToAnnotation {
    fn to_annotation(&self) -> Annotation;
}

// this suggests we're using LocatedSpan wrong
impl<'a> ToAnnotation for LocatedSpan<'a> {
    fn to_annotation(&self) -> Annotation {
        let start_column = self.get_utf8_column();
        let start_row = self.location_line();

        let fragment_length = self.fragment().len();

        let end_column = start_column + fragment_length;

        let start_offset = self.location_offset();

        Annotation {
            start: Position {
                offset: start_offset,
                row: start_row,
                column: start_column,
            },
            end: Position {
                offset: start_offset + fragment_length,
                row: start_row,
                column: end_column,
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub ann: Annotation,
    pub message: String,
}

#[derive(Clone, Debug)]
pub struct State<'a>(pub &'a RefCell<Vec<ParseError>>);

impl<'a> State<'a> {
    pub fn report_error(&self, error: ParseError) {
        self.0.borrow_mut().push(error);
    }
}

#[derive(Debug)]
pub struct ParseBlock {
    pub let_bindings: Vec<(Annotation, Option<String>, ParseExpr)>,
    pub final_expr: ParseExpr,
}

#[derive(Debug)]
pub enum ParseExpr {
    Ident {
        ann: Annotation,
        var: String,
    },
    Prim {
        ann: Annotation,
        prim: Prim,
    },
    If {
        ann: Annotation,
        pred_expr: Box<ParseExpr>,
        then_expr: Box<ParseExpr>,
        else_expr: Box<ParseExpr>,
    },
    Ann {
        ann: Annotation,
        ty: Option<ParseType>,
        expr: Box<ParseExpr>,
    },
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseType {
    Prim {
        ann: Annotation,
        type_prim: TypePrim,
    },
}
