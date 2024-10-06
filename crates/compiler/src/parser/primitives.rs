pub use super::types::{IResult, LocatedSpan, ParseExpr};

use super::combinators::{with_annotation, ws};
use crate::types::{Prim, TypePrim};
use nom::branch::alt;
use nom::combinator::map;
use nom::{
    bytes::complete::{tag, take_while_m_n},
    combinator::map_res,
};

// Need to add more types as we need them pls
pub fn parse_type_prim(input: LocatedSpan) -> IResult<TypePrim> {
    let boolean = map(tag("Boolean"), |_| TypePrim::TBoolean);
    let int_8 = map(tag("Int8"), |_| TypePrim::TInt8);
    let int_16 = map(tag("Int16 "), |_| TypePrim::TInt16);
    let int_32 = map(tag("Int32"), |_| TypePrim::TInt32);
    let int_64 = map(tag("Int64"), |_| TypePrim::TInt64);
    ws(alt((boolean, int_8, int_16, int_32, int_64)))(input)
}

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

pub fn prim(input: LocatedSpan) -> IResult<ParseExpr> {
    map(ws(with_annotation(alt((bool, int)))), |(ann, prim)| {
        ParseExpr::Prim { ann, prim }
    })(input)
}
