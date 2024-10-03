use std::cell::RefCell;
use std::ops::Range;

use crate::types::{Prim, TypePrim};

pub type IResult<'a, T> = nom::IResult<LocatedSpan<'a>, T>;

pub type LocatedSpan<'a> = nom_locate::LocatedSpan<&'a str, State<'a>>;

#[derive(Debug, Clone, PartialEq)]
pub struct Annotation<Span> {
    pub start: Span,
    pub end: Span,
}

pub type ParseAnnotation<'a> = Annotation<LocatedSpan<'a>>;

pub type StaticAnnotation<'a> = Annotation<nom_locate::LocatedSpan<&'a str, ()>>;

pub trait ToRange {
    fn to_range(&self) -> Range<usize>;
}

// this suggests we're using LocatedSpan wrong
impl<'a> ToRange for LocatedSpan<'a> {
    fn to_range(&self) -> Range<usize> {
        let start = self.location_offset();
        let end = start + self.fragment().len();
        start..end
    }
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub range: Range<usize>,
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
pub enum ParseExpr<'a> {
    Ident {
        ann: ParseAnnotation<'a>,
        var: String,
    },
    Prim {
        ann: ParseAnnotation<'a>,
        prim: Prim,
    },
    If {
        ann: ParseAnnotation<'a>,
        pred_expr: Box<ParseExpr<'a>>,
        then_expr: Box<ParseExpr<'a>>,
        else_expr: Box<ParseExpr<'a>>,
    },
    Ann {
        ann: ParseAnnotation<'a>,
        ty: Option<ParseType<'a>>,
        expr: Box<ParseExpr<'a>>,
    },
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseType<'a> {
    Prim {
        ann: ParseAnnotation<'a>,
        type_prim: TypePrim,
    },
}
