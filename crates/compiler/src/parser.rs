pub mod constructors;
mod convert;
mod types;

pub use convert::{to_real_expr, to_real_ty};
use std::cell::RefCell;
pub use types::{
    Annotation, IResult, LocatedSpan, ParseError, ParseExpr, ParseType, Position, State,
    ToAnnotation,
};

use crate::types::{Prim, TypePrim};
use nom::branch::alt;
use nom::bytes::complete::{take, take_till1};
use nom::character::complete::{anychar, multispace0};
use nom::combinator::{all_consuming, map, map_opt, not, recognize, rest, verify};
use nom::sequence::{preceded, terminated};
use nom::{
    bytes::complete::{tag, take_while, take_while_m_n},
    combinator::map_res,
};

// given a parser, return a parser that returns the result and an Annotation describing the source
// location
pub fn with_annotation<'a, F, T>(
    mut inner: F,
) -> impl FnMut(LocatedSpan<'a>) -> IResult<(Annotation, T)>
where
    F: FnMut(LocatedSpan<'a>) -> IResult<T>,
{
    move |input: LocatedSpan| {
        let (input, start) = nom_locate::position(input)?;
        let (input, result) = inner(input)?;
        let (input, end) = nom_locate::position(input)?;

        let ann = Annotation {
            start: Position {
                offset: start.location_offset(),
                row: start.location_line(),
                column: start.get_utf8_column(),
            },
            end: Position {
                offset: end.location_offset(),
                row: end.location_line(),
                column: end.get_utf8_column(),
            },
        };
        Ok((input, (ann, result)))
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
            let err = ParseError {
                ann: input.to_annotation(),
                message: error_msg.to_string(),
            };
            input.extra.report_error(err);
            Ok((input, None))
        }
        Err(err) => Err(err),
    }
}

fn ident_inner(input: LocatedSpan) -> IResult<String> {
    let first = verify(anychar, |c| c.is_ascii_alphabetic() || *c == '_');
    let rest = take_while(|c: char| c.is_ascii_alphanumeric() || "_-'".contains(c));
    let ident = recognize(preceded(first, rest));
    map_opt(ident, |span: LocatedSpan| {
        let str = span.fragment().to_string();

        let protected: [&str; 6] = ["if", "then", "else", "let", "True", "False"];

        if protected.contains(&str.as_str()) {
            None
        } else {
            Some(str)
        }
    })(input)
}

fn ident(input: LocatedSpan) -> IResult<ParseExpr> {
    let parser = ws(with_annotation(ident_inner));
    map(parser, |(ann, var): (Annotation, String)| {
        ParseExpr::Ident { ann, var }
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

struct ParsedIf {
    pred_expr: ParseExpr,
    then_expr: ParseExpr,
    else_expr: ParseExpr,
}

fn parse_if_internal(input: LocatedSpan) -> IResult<ParsedIf> {
    let (input, _) = ws(tag("if"))(input)?;
    let (input, pred_expr) = expect(expr, "expected expression after 'if'")(input)?;

    let (input, _) = expect(ws(tag("then")), "expected 'then'")(input)?;
    let (input, then_expr) = expect(expr, "expected expression after 'then'")(input)?;

    let (input, _) = expect(ws(tag("else")), "expected 'else'")(input)?;
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

fn let_internal(
    input: LocatedSpan,
) -> IResult<(Option<String>, Option<ParseExpr>, Option<ParseExpr>)> {
    let (input, _) = ws(tag("let"))(input)?;
    let (input, ident) = expect(ws(ident_inner), "expected identifier")(input)?;

    let (input, _) = expect(ws(tag("=")), "expected '='")(input)?;
    let (input, exp) = expect(expr, "expected expression after '='")(input)?;

    let (input, _) = ws(tag(";"))(input)?;
    let (input, rest) = expect(expr, "expected next expression")(input)?;

    Ok((input, (ident, exp, rest)))
}

fn lett(input: LocatedSpan) -> IResult<ParseExpr> {
    let (input, (ann, (var, expr, rest))) = with_annotation(let_internal)(input)?;

    Ok((
        input,
        ParseExpr::Let {
            ann,
            var,
            expr: Box::new(expr.unwrap_or(ParseExpr::Error)),
            rest: Box::new(rest.unwrap_or(ParseExpr::Error)),
        },
    ))
}

fn error(input: LocatedSpan) -> IResult<ParseExpr> {
    map(take_till1(|c| c == ')'), |span: LocatedSpan| {
        let err = ParseError {
            ann: span.to_annotation(),
            message: format!("unexpected `{}`", span.fragment()),
        };
        span.extra.report_error(err);
        ParseExpr::Error
    })(input)
}

// our main Expr parser, basically, try all the parsers
fn expr(input: LocatedSpan) -> IResult<ParseExpr> {
    alt((ann, lett, prim, iff, ident, error))(input)
}

// parse a whole source file
fn source_file(input: LocatedSpan) -> IResult<ParseExpr> {
    let expr = alt((expr, map(take(0usize), |_| ParseExpr::Error)));
    terminated(expr, preceded(expect(not(anychar), "expected EOF"), rest))(input)
}

pub fn parse<'a, 'state>(source: &'a str) -> (ParseExpr, Vec<ParseError>)
where
    'state: 'a,
{
    let errors = RefCell::new(vec![]);
    let input = LocatedSpan::new_extra(source, State(&errors));
    let (_, expr) = all_consuming(source_file)(input).expect("parser cannot fail");
    let copied_errors = errors.clone().into_inner().clone();

    (expr, copied_errors)
}

pub fn parse_type<'a, 'state>(source: &'a str) -> (ParseType, Vec<ParseError>)
where
    'state: 'a,
{
    let errors = RefCell::new(vec![]);
    let input = LocatedSpan::new_extra(source, State(&errors));
    let (_, ty) = all_consuming(ty)(input).expect("parser cannot fail");
    (ty, errors.clone().into_inner())
}

pub fn main() {
    for input in &["foo", "(foo)", "(foo))", "(%", "(", "%", "()", ""] {
        println!("{:7} {:?}", input, parse(input));
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
