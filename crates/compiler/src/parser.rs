pub mod constructors;

use std::cell::RefCell;
use std::ops::Range;

use crate::types::{Expr, Prim, Type, TypePrim};
use nom::branch::alt;
use nom::bytes::complete::{take, take_till1, take_while};
use nom::character::complete::{anychar, multispace0};
use nom::combinator::{all_consuming, map, not, recognize, rest, verify};
use nom::sequence::{preceded, terminated};
use nom::{
    bytes::complete::{tag, take_while_m_n},
    combinator::map_res,
};

type IResult<'a, T> = nom::IResult<LocatedSpan<'a>, T>;

pub type LocatedSpan<'a> = nom_locate::LocatedSpan<&'a str, State<'a>>;

#[derive(Debug, Clone, PartialEq)]
pub struct Annotation<Span> {
    pub start: Span,
    pub end: Span,
}

type ParseAnnotation<'a> = Annotation<LocatedSpan<'a>>;

pub type StaticAnnotation<'a> = Annotation<nom_locate::LocatedSpan<&'a str, ()>>;

trait ToRange {
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
pub struct Error<Span> {
    pub annotation: Annotation<Span>,
    pub message: String,
}

pub type ParseError<'a> = Error<LocatedSpan<'a>>;

pub type StaticParseError<'a> = Error<nom_locate::LocatedSpan<&'a str, ()>>;

#[derive(Clone, Debug)]
pub struct State<'a>(&'a RefCell<Vec<ParseError<'a>>>);

impl<'a> State<'a> {
    pub fn report_error(&self, error: ParseError<'a>) {
        self.0.borrow_mut().push(error);
    }
}

// given a parser, return a parser that returns the result and an Annotation describing the source
// location
pub fn with_annotation<'a, F, T>(
    mut inner: F,
) -> impl FnMut(LocatedSpan<'a>) -> IResult<(ParseAnnotation<'a>, T)>
where
    F: FnMut(LocatedSpan<'a>) -> IResult<T>,
{
    move |input: LocatedSpan| {
        let (input, start) = nom_locate::position(input)?;
        let (input, result) = inner(input)?;
        let (input, end) = nom_locate::position(input)?;
        Ok((input, (Annotation { start, end }, result)))
    }
}

fn expect<'a, F, E, T>(
    mut parser: F,
    error_msg: E,
) -> impl FnMut(LocatedSpan<'a>) -> IResult<Option<T>>
where
    F: FnMut(LocatedSpan<'a>) -> IResult<T>,
    E: ToString,
{
    move |input| match parser(input) {
        Ok((remaining, out)) => Ok((remaining, Some(out))),
        Err(nom::Err::Error(nom::error::Error { input, .. }))
        | Err(nom::Err::Failure(nom::error::Error { input, .. })) => {
            let err = Error {
                annotation: Annotation {
                    start: input.clone(),
                    end: input.clone(),
                },
                message: error_msg.to_string(),
            };
            input.extra.report_error(err);
            Ok((input, None))
        }
        Err(err) => Err(err),
    }
}

#[derive(Debug)]
pub struct Ident(String);

#[derive(Debug)]
pub enum ParseExpr<'a> {
    Ident {
        ann: ParseAnnotation<'a>,
        ident: Ident,
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

fn ident(input: LocatedSpan) -> IResult<ParseExpr> {
    let first = verify(anychar, |c| c.is_ascii_alphabetic() || *c == '_');
    let rest = take_while(|c: char| c.is_ascii_alphanumeric() || "_-'".contains(c));
    let ident = ws(with_annotation(recognize(preceded(first, rest))));
    map(ident, |(ann, span): (ParseAnnotation, LocatedSpan)| {
        ParseExpr::Ident {
            ann,
            ident: Ident(span.fragment().to_string()),
        }
    })(input)
}

// Need to add more types as we need them pls
fn parse_type_prim(input: LocatedSpan) -> IResult<TypePrim> {
    let boolean = map(tag("Boolean"), |_| TypePrim::TBoolean);
    let int_8 = map(tag("Int8"), |_| TypePrim::TInt8);
    let int_16 = map(tag("Int16 "), |_| TypePrim::TInt16);
    let int_32 = map(tag("Int32"), |_| TypePrim::TInt32);
    let int_64 = map(tag("Int64"), |_| TypePrim::TInt64);
    ws(alt((boolean, int_8, int_16, int_32, int_64)))(input)
}

pub fn ty(input: LocatedSpan) -> IResult<ParseType> {
    let (input, (ann, type_prim)) = ws(with_annotation(parse_type_prim))(input)?;
    Ok((input, ParseType::Prim { ann, type_prim }))
}

fn iff(input: LocatedSpan) -> IResult<ParseExpr> {
    let (
        input,
        (
            ann,
            ParsedIf {
                pred_expr,
                then_expr,
                else_expr,
            },
        ),
    ) = ws(with_annotation(parse_if_internal))(input)?;

    Ok((
        input,
        ParseExpr::If {
            ann,
            pred_expr: Box::new(pred_expr),
            then_expr: Box::new(then_expr),
            else_expr: Box::new(else_expr),
        },
    ))
}

struct ParsedIf<'a> {
    pred_expr: ParseExpr<'a>,
    then_expr: ParseExpr<'a>,
    else_expr: ParseExpr<'a>,
}

fn parse_if_internal(input: LocatedSpan) -> IResult<ParsedIf> {
    let (input, _) = tag("if")(input)?;
    let (input, pred_expr) = expect(expr, "expected expression after 'if'")(input)?;

    let (input, _) = ws(tag("then"))(input)?;
    let (input, then_expr) = expect(expr, "expected expression after 'then'")(input)?;

    let (input, _) = ws(tag("else"))(input)?;
    let (input, else_expr) = expect(expr, "expected expression after 'else'")(input)?;

    Ok((
        input,
        ParsedIf {
            pred_expr: pred_expr.unwrap_or(ParseExpr::Error),
            then_expr: then_expr.unwrap_or(ParseExpr::Error),
            else_expr: else_expr.unwrap_or(ParseExpr::Error),
        },
    ))
}

fn error(input: LocatedSpan) -> IResult<ParseExpr> {
    map(take_till1(|c| c == ')'), |span: LocatedSpan| {
        let err = Error {
            annotation: Annotation {
                start: span.clone(),
                end: span.clone(),
            },
            message: format!("unexpected `{}`", span.fragment()),
        };
        span.extra.report_error(err);
        ParseExpr::Error
    })(input)
}

// our main Expr parser, basically, try all the parsers
fn expr(input: LocatedSpan) -> IResult<ParseExpr> {
    alt((ann, prim, iff, ident, error))(input)
}

// parse a whole source file
fn source_file(input: LocatedSpan) -> IResult<ParseExpr> {
    let expr = alt((expr, map(take(0usize), |_| ParseExpr::Error)));
    terminated(expr, preceded(expect(not(anychar), "expected EOF"), rest))(input)
}

pub fn parse<'a, 'state>(
    errors: &'state RefCell<Vec<ParseError<'a>>>,
    source: &'a str,
) -> (ParseExpr<'a>, Vec<ParseError<'a>>)
where
    'state: 'a,
{
    let input = LocatedSpan::new_extra(source, State(&errors));
    let (_, expr) = all_consuming(source_file)(input).expect("parser cannot fail");
    let copied_errors = errors.clone().into_inner().clone();

    (expr, copied_errors)
}

pub fn parse_type<'a, 'state>(
    errors: &'state RefCell<Vec<ParseError<'a>>>,
    source: &'a str,
) -> (ParseType<'a>, Vec<ParseError<'a>>)
where
    'state: 'a,
{
    let input = LocatedSpan::new_extra(source, State(errors));
    let (_, ty) = all_consuming(ty)(input).expect("parser cannot fail");
    (ty, errors.clone().into_inner())
}

pub fn main() {
    for input in &["foo", "(foo)", "(foo))", "(%", "(", "%", "()", ""] {
        let errors = RefCell::new(Vec::new());
        println!("{:7} {:?}", input, parse(&errors, input));
    }
}

//////// prims

fn is_int_digit(c: char) -> bool {
    c.is_ascii_digit()
}

fn int_primary(input: LocatedSpan) -> IResult<u8> {
    map_res(take_while_m_n(1, 12, is_int_digit), from_int)(input)
}

fn from_int(input: LocatedSpan) -> Result<u8, std::num::ParseIntError> {
    input.parse::<u8>()
}

fn int(input: LocatedSpan) -> IResult<Prim> {
    let (input, int_val) = int_primary(input)?;
    Ok((input, Prim::IntLit(i64::from(int_val))))
}

fn parse_true(input: LocatedSpan) -> IResult<Prim> {
    map(tag("True"), |_| Prim::Boolean(true))(input)
}

fn parse_false(input: LocatedSpan) -> IResult<Prim> {
    map(tag("False"), |_| Prim::Boolean(false))(input)
}

fn bool(input: LocatedSpan) -> IResult<Prim> {
    (alt((parse_true, parse_false)))(input)
}

fn prim(input: LocatedSpan) -> IResult<ParseExpr> {
    map(ws(with_annotation(alt((bool, int)))), |(ann, prim)| {
        ParseExpr::Prim { ann, prim }
    })(input)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
pub fn ws<'a, F, T>(inner: F) -> impl FnMut(LocatedSpan<'a>) -> IResult<T>
where
    F: FnMut(LocatedSpan<'a>) -> IResult<T>,
{
    preceded(multispace0, inner)
}

fn parse_ann_internal(input: LocatedSpan) -> IResult<(Option<ParseType>, Option<ParseExpr>)> {
    let (input, _) = tag("(")(input)?;

    let (input, expr) = expect(expr, "expected expression after '('")(input)?;

    let (input, _) = expect(ws(tag(":")), "expected ':'")(input)?;

    let (input, ty) = expect(ty, "expected type after ':'")(input)?;

    let (input, _) = expect(tag(")"), "expected closing ')'")(input)?;

    Ok((input, (ty, expr)))
}

fn ann(input: LocatedSpan) -> IResult<ParseExpr> {
    let (input, (ann, (ty, expr))) = ws(with_annotation(parse_ann_internal))(input)?;

    Ok((
        input,
        ParseExpr::Ann {
            ann,
            ty,
            expr: Box::new(expr.unwrap_or(ParseExpr::Error)),
        },
    ))
}

#[derive(Debug, PartialEq)]
pub enum ParseConvertError {
    FoundError,
    FoundIdent,
    MissingTypeAnnotation,
}

// turn this into our 'real' expr that does not include `Error`
// so we can typecheck it and generally try it out
pub fn to_real_expr(parse_expr: ParseExpr) -> Result<Expr<StaticAnnotation>, ParseConvertError> {
    match parse_expr {
        ParseExpr::Ann { ann, ty, expr } => Ok(Expr::EAnn {
            ann: to_real_ann(ann),
            ty: to_real_ty(ty)?,
            expr: Box::new(to_real_expr(*expr)?),
        }),
        ParseExpr::Prim { ann, prim } => Ok(Expr::EPrim {
            ann: to_real_ann(ann),
            prim,
        }),
        ParseExpr::Ident { .. } => Err(ParseConvertError::FoundIdent),
        ParseExpr::If {
            ann,
            pred_expr,
            then_expr,
            else_expr,
        } => Ok(Expr::EIf {
            ann: to_real_ann(ann),
            pred_expr: Box::new(to_real_expr(*pred_expr)?),
            then_expr: Box::new(to_real_expr(*then_expr)?),
            else_expr: Box::new(to_real_expr(*else_expr)?),
        }),
        ParseExpr::Error => Err(ParseConvertError::FoundError),
    }
}

pub fn to_real_ty(
    parse_ty: Option<ParseType>,
) -> Result<Type<StaticAnnotation>, ParseConvertError> {
    parse_ty
        .map(|ty| match ty {
            ParseType::Prim { ann, type_prim } => Type::TPrim {
                ann: to_real_ann(ann),
                type_prim,
            },
        })
        .ok_or(ParseConvertError::MissingTypeAnnotation)
}

fn to_real_ann(ann: ParseAnnotation) -> StaticAnnotation {
    Annotation {
        start: ann.start.map_extra(|_| ()),
        end: ann.end.map_extra(|_| ()),
    }
}

pub fn to_real_error(error: ParseError) -> StaticParseError {
    Error {
        annotation: to_real_ann(error.annotation),
        message: error.message,
    }
}
