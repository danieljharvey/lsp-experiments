pub use super::types::{Annotation, IResult, LocatedSpan, ParseError, Position, ToAnnotation};

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

pub fn expect<'a, F, E, T>(
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

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
pub fn ws<'a, F, T>(inner: F) -> impl FnMut(LocatedSpan<'a>) -> IResult<T>
where
    F: FnMut(LocatedSpan<'a>) -> IResult<T>,
{
    nom::sequence::preceded(nom::character::complete::multispace0, inner)
}
