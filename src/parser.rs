use crate::types::{Expr, Prim, Type, TypePrim};
use nom::branch::alt;
use nom::{
    bytes::complete::{tag, take_while_m_n},
    combinator::{map, map_res},
    IResult,
};

use nom::{character::complete::multispace0, error::ParseError, sequence::preceded};

use nom_locate::{position, LocatedSpan};

type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, PartialEq)]
pub struct Annotation<'a> {
    pub start: Span<'a>,
    pub end: Span<'a>,
}

// construct int
pub fn int<Ann>(ann: Ann, int_val: i64) -> Expr<Ann> {
    Expr::EPrim {
        ann,
        prim: Prim::IntLit(int_val),
    }
}

// construct bool
pub fn bool<Ann>(ann: Ann, bool_val: bool) -> Expr<Ann> {
    Expr::EPrim {
        ann,
        prim: Prim::Boolean(bool_val),
    }
}

/*
// construct var
pub fn var(identifier: Span) -> Expr<()> {
    Expr::EVar {
        ann: (),
        identifier: identifier.to_string(),
    }
}
*/

// construct if
pub fn mk_if<Ann>(
    ann: Ann,
    pred_expr: Expr<Ann>,
    then_expr: Expr<Ann>,
    else_expr: Expr<Ann>,
) -> Expr<Ann> {
    Expr::EIf {
        ann,
        pred_expr: Box::new(pred_expr),
        then_expr: Box::new(then_expr),
        else_expr: Box::new(else_expr),
    }
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
pub fn ws<'a, F, O, E: ParseError<Span<'a>>>(
    inner: F,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, O, E>
where
    F: FnMut(Span<'a>) -> IResult<Span<'a>, O, E>,
{
    preceded(multispace0, inner)
}

// given a parser, return a parser that returns the result and an Annotation describing the source
// location
pub fn with_annotation<'a, F, O, E: ParseError<Span<'a>>>(
    mut inner: F,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, (Annotation<'a>, O), E>
where
    F: FnMut(Span<'a>) -> IResult<Span<'a>, O, E>,
{
    move |input: Span| {
        let (input, start) = position(input)?;
        let (input, result) = inner(input)?;
        let (input, end) = position(input)?;
        Ok((input, (Annotation { start, end }, result)))
    }
}

fn is_int_digit(c: char) -> bool {
    c.is_ascii_digit()
}

fn int_primary(input: Span) -> IResult<Span, u8> {
    map_res(take_while_m_n(1, 12, is_int_digit), from_int)(input)
}

fn from_int(input: Span) -> Result<u8, std::num::ParseIntError> {
    input.parse::<u8>()
}

fn parse_my_int(input: Span) -> IResult<Span, Expr<Annotation>> {
    let (input, (annotation, int_val)) = ws(with_annotation(int_primary))(input)?;
    Ok((input, int(annotation, i64::from(int_val))))
}

fn parse_ann_internal(input: Span) -> IResult<Span, (Type<Annotation>, Expr<Annotation>)> {
    let (input, _) = tag("(")(input)?;

    let (input, expr) = parse_expr(input)?;

    let (input, _) = ws(tag(":"))(input)?;

    let (input, ty) = parse_type(input)?;

    let (input, _) = tag(")")(input)?;

    Ok((input, (ty, expr)))
}

fn parse_ann(input: Span) -> IResult<Span, Expr<Annotation>> {
    let (input, (ann, (ty, expr))) = ws(with_annotation(parse_ann_internal))(input)?;

    Ok((
        input,
        Expr::EAnn {
            ann,
            ty,
            expr: Box::new(expr),
        },
    ))
}

// Need to add more types as we need them pls
fn parse_type_prim(input: Span) -> IResult<Span, TypePrim> {
    let boolean = map(tag("Boolean"), |_| TypePrim::TBoolean);
    let int_8 = map(tag("Int8"), |_| TypePrim::TInt8);
    let int_16 = map(tag("Int16 "), |_| TypePrim::TInt16);
    let int_32 = map(tag("Int32"), |_| TypePrim::TInt32);
    let int_64 = map(tag("Int64"), |_| TypePrim::TInt64);
    ws(alt((boolean, int_8, int_16, int_32, int_64)))(input)
}

pub fn parse_type(input: Span) -> IResult<Span, Type<Annotation>> {
    let (input, (ann, type_prim)) = ws(with_annotation(parse_type_prim))(input)?;
    Ok((input, Type::TPrim { ann, type_prim }))
}

/*
// check we aren't using protected words for variables
fn var_is_protected(ident: Span) -> bool {
    vec!["True", "False", "if", "then", "else"].contains(&ident)
}

// jesus
fn parse_my_var(input: Span) -> IResult<Span, Expr<()>> {
    map_res(ws(alpha1), |var_val| {
        match var_is_protected(var_val) {
            true => Err(nom::Err::Error {
                0: nom::error::Error {
                    code: nom::error::ErrorKind::Tag,
                    input: input,
                },
            }),
            false => Ok(var(var_val)),
        }
    })(input)
}

#[test]
fn test_parse_my_var() {
    assert_ne!(parse_my_var("False"), Ok(("", var("False"))));
    assert_ne!(parse_my_var("True"), Ok(("", var("True"))));
    assert_ne!(parse_my_var("if"), Ok(("", var("if"))));
    assert_eq!(parse_my_var(" p"), Ok(("", var("p"))));
    assert_eq!(parse_my_var("p"), Ok(("", var("p"))));
    assert_eq!(parse_my_var("poo"), Ok(("", var("poo"))));
    assert_eq!(parse_my_var("poo "), Ok((" ", var("poo"))))
}
*/

fn parse_true(input: Span) -> IResult<Span, Expr<Annotation>> {
    map(ws(with_annotation(tag("True"))), |(ann, _)| bool(ann, true))(input)
}

fn parse_false(input: Span) -> IResult<Span, Expr<Annotation>> {
    map(ws(with_annotation(tag("False"))), |(ann, _)| {
        bool(ann, false)
    })(input)
}

fn parse_my_bool(input: Span) -> IResult<Span, Expr<Annotation>> {
    alt((parse_true, parse_false))(input)
}

struct ParsedIf<Ann> {
    pred_expr: Expr<Ann>,
    then_expr: Expr<Ann>,
    else_expr: Expr<Ann>,
}

fn parse_if_internal(input: Span) -> IResult<Span, ParsedIf<Annotation>> {
    let (input, _) = tag("if")(input)?;
    let (input, pred_expr) = parse_expr(input)?;

    let (input, _) = ws(tag("then"))(input)?;
    let (input, then_expr) = parse_expr(input)?;

    let (input, _) = ws(tag("else"))(input)?;
    let (input, else_expr) = parse_expr(input)?;

    Ok((
        input,
        ParsedIf {
            pred_expr,
            then_expr,
            else_expr,
        },
    ))
}

fn parse_my_if(input: Span) -> IResult<Span, Expr<Annotation>> {
    let (
        input,
        (
            annotation,
            ParsedIf {
                pred_expr,
                then_expr,
                else_expr,
            },
        ),
    ) = ws(with_annotation(parse_if_internal))(input)?;

    Ok((input, mk_if(annotation, pred_expr, then_expr, else_expr)))
}

pub fn parse_expr(input: Span) -> IResult<Span, Expr<Annotation>> {
    alt((parse_my_int, parse_my_bool, parse_my_if, parse_ann))(input)
}
