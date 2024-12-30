mod combinators;

mod primitives;
use primitives::{parse_type_prim, prim};
pub mod constructors;

mod convert;
mod parse_error;
mod types;
use combinators::{expect, with_annotation, ws};
pub use convert::{parse_block_to_expr, parse_function_to_function, to_real_expr, to_real_ty};
use nom::branch::alt;
use nom::bytes::complete::{take, take_till1};
use nom::character::complete::anychar;
use nom::combinator::{all_consuming, map, map_opt, not, opt, recognize, rest, verify};
use nom::sequence::{preceded, terminated};
use nom::{
    bytes::complete::{tag, take_while},
    multi::separated_list1,
};
pub use parse_error::to_report;
use std::cell::RefCell;
pub use types::{
    Annotation, IResult, LocatedSpan, ParseBlock, ParseError, ParseExpr, ParseFunction, ParseType,
    Position, State, ToAnnotation,
};

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

fn error(input: LocatedSpan) -> IResult<ParseExpr> {
    map(take_till1(|c| c == ')' || c == ';'), |span: LocatedSpan| {
        let err = ParseError {
            ann: span.to_annotation(),
            message: format!("unexpected `{}`", span.fragment()),
        };
        span.extra.report_error(err);
        ParseExpr::Error
    })(input)
}

fn skip_till_end_of_binding(input: LocatedSpan) -> IResult<(Option<String>, ParseExpr)> {
    map(take_till1(|c| c == ';'), |span: LocatedSpan| {
        let err = ParseError {
            ann: span.to_annotation(),
            message: format!("unexpected `{}`", span.fragment()),
        };
        span.extra.report_error(err);
        (None, ParseExpr::Error)
    })(input)
}

// our main Expr parser, basically, try all the parsers
fn expr(input: LocatedSpan) -> IResult<ParseExpr> {
    alt((ann, prim, iff, ident, error))(input)
}

// try to parse block, or return result of `error` parser, which
// will eat until `;` so we can get on with our lives
fn let_block_internal(input: LocatedSpan) -> IResult<(Option<String>, ParseExpr)> {
    let (input, ident) = expect(ws(ident_inner), "expected identifier")(input)?;

    let (input, _) = expect(ws(tag("=")), "expected '='")(input)?;
    let (input, exp) = expr(input)?;

    Ok((input, (ident, exp)))
}

fn function(input: LocatedSpan) -> IResult<ParseFunction> {
    let (input, name) = preceded(ws(tag("fun")), ident_inner)(input)?;

    let (input, block) = block(input)?;
    Ok((
        input,
        ParseFunction {
            name,
            arguments: vec![],
            return_type: None,
            block,
        },
    ))
}

fn block(input: LocatedSpan) -> IResult<ParseBlock> {
    let let_binding_parser = with_annotation(preceded(
        ws(tag("let")),
        alt((let_block_internal, skip_till_end_of_binding)),
    ));

    // optional list of let bindings
    let (input, let_bindings) = opt(terminated(
        separated_list1(tag(";"), let_binding_parser),
        tag(";"),
    ))(input)?;

    // expression
    let (input, exp) = expr(input)?;

    Ok((
        input,
        ParseBlock {
            let_bindings: match let_bindings {
                Some(bindings) => bindings
                    .into_iter()
                    .map(|(ann, (ident, expr_body))| (ann, ident, expr_body))
                    .collect(),
                None => vec![],
            },
            final_expr: exp,
        },
    ))
}

// parse a whole source file
fn source_file(input: LocatedSpan) -> IResult<ParseFunction> {
    let parse_block = alt((
        function,
        map(take(0usize), |_| ParseFunction {
            name: String::new(),
            return_type: None,
            arguments: vec![],
            block: ParseBlock {
                let_bindings: vec![],
                final_expr: ParseExpr::Error,
            },
        }),
    ));
    terminated(parse_block, preceded(opt(not(anychar)), rest))(input)
}

// file with one expr in it
fn source_block(input: LocatedSpan) -> IResult<ParseBlock> {
    let parse_block = alt((
        block,
        map(take(0usize), |_| ParseBlock {
            let_bindings: vec![],
            final_expr: ParseExpr::Error,
        }),
    ));
    terminated(parse_block, preceded(opt(not(anychar)), rest))(input)
}

pub fn parse_function<'a, 'state>(source: &'a str) -> (ParseFunction, Vec<ParseError>)
where
    'state: 'a,
{
    let errors = RefCell::new(vec![]);
    let input = LocatedSpan::new_extra(source, State(&errors));
    let (_, function) = all_consuming(source_file)(input).expect("parser cannot fail");
    let copied_errors = errors.clone().into_inner().clone();

    (function, copied_errors)
}

pub fn parse_expr<'a, 'state>(source: &'a str) -> (ParseBlock, Vec<ParseError>)
where
    'state: 'a,
{
    let errors = RefCell::new(vec![]);
    let input = LocatedSpan::new_extra(source, State(&errors));
    let (_, expr) = all_consuming(source_block)(input).expect("parser cannot fail");
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
