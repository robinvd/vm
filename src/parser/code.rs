use std::collections::HashMap;

use combine::char::{letter, string};
use combine::range::{recognize, take_while, take_while1};
use combine::{
    attempt, between, choice, eof, many, many1, one_of, optional, sep_by, sep_by1, ParseError,
    Parser, RangeStream, any
};

use crate::parser::*;
use crate::vm::value::{GCObject, Object, Value};

#[derive(Debug, PartialEq)]
pub struct Function<'a> {
    pub name: &'a str,
    pub args: Vec<&'a str>,
    pub n_returns: usize,
    pub body: Vec<Expr<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct BcFunction<'a> {
    pub name: &'a str,
    pub args: Vec<&'a str>,
    pub body: Vec<bytecode::Instr<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum TopLevel<'a> {
    Function(Function<'a>),
    BcFunction(BcFunction<'a>),
    // Use(UseTree<'a>),
    Use(Vec<&'a str>),
}

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    Lit(Value),
    Call(&'a str, Vec<Expr<'a>>),
    Tuple(Vec<Expr<'a>>),
    Var(&'a str),
    While(Box<Expr<'a>>, Vec<Expr<'a>>),
    If(Box<Expr<'a>>, Vec<Expr<'a>>, Option<Vec<Expr<'a>>>),
    Let(Vec<&'a str>, Vec<Expr<'a>>),
    Assign(&'a str, Box<Expr<'a>>),
    Return(Vec<Expr<'a>>),
    Index(Box<Expr<'a>>, Box<Expr<'a>>),
}

pub fn skip_whitespace<'a, I>() -> impl Parser<Input = I, Output = &'a str>
where
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    take_while(|c| is_space(c) || c == '\n')
}

pub fn ident<'a, I>() -> impl Parser<Input = I, Output = &'a str>
where
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let parser = (letter(), take_while(|c: char| c.is_alphanumeric() || c == '_'));
    recognize(parser).skip(skip_whitespace())
}

parser!{
    pub fn int['a, I]()(I) -> isize
    where [
        I: RangeStream<Item = char, Range = &'a str>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        take_while1(|c: char| is_num(c))
            .skip(skip_whitespace())
            .map(|s: &str| s.parse::<isize>().unwrap())
    }
}

parser!{
    pub fn float['a, I]()(I) -> f64
    where [
        I: RangeStream<Item = char, Range = &'a str>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        recognize((
            take_while1(|c: char| is_num(c)),
            optional((
                string("."),
                take_while1(|c: char| is_num(c)),
            ))
        ))
            .skip(skip_whitespace())
            .map(|s: &str| s.parse::<f64>().unwrap())
    }
}

pub fn use_parse<'a, I>() -> impl Parser<Input = I, Output = TopLevel<'a>>
where
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (text("use"), sep_by1(ident(), text("::")), text(";")).map(|(_, vals, _)| TopLevel::Use(vals))
}

parser!{
    pub fn text['a, I](s: &'static str)(I) -> &'a()
    where [
        I: RangeStream<Item = char, Range = &'a str>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        attempt(string(s).skip(skip_whitespace()).map(|_| &()))
    }
}

parser!{
    pub fn arg_list['a, I, P, F, R](x: F)(I) -> Vec<R>
    where [
        I: RangeStream<Item = char, Range = &'a str>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
        P: Parser<Input=I, Output=R>,
        F: FnMut() -> P,
    ]
    {
        between(text("("), text(")"), sep_by1(x(), text(",")))
    }
}

parser!{
    pub fn literal['a, I]()(I) -> Value
    where [
        I: RangeStream<Item = char, Range = &'a str>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        choice((
            float().map(Value::Number),
            text("True").map(|_| Value::True),
            text("False").map(|_| Value::False),
            text("Nil").map(|_| Value::Nil),
            between(
                text("["), text("]"),
                sep_by(literal(), text(","))
            ).map(|ls| Value::Object(GCObject::new(Object::List(ls)))),
            between(
                text("{"), text("}"),
                sep_by((literal().skip(text("=")), literal()), text(","))
            ).map(|ls: Vec<(Value, Value)>| {
                let mut hm = HashMap::new();
                for (k, v) in ls {
                    hm.insert(k,v);
                }
                Value::Object(GCObject::new(Object::Map(hm)))
            }),
            between(
                text("\""), text("\""),
                take_while(|c| c != '"'),
            ).map(|s: &str| Value::Object(GCObject::new(Object::String(s.to_owned())))),
            between(
                text("'"), text("'"),
                any(),
            ).map(|c: char| Value::Number(c as i64 as f64)),
        ))
    }
}

fn expr_<'a, I>() -> impl Parser<Input = I, Output = Expr<'a>>
where
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let call = || {
        (
            ident(),
            between(text("("), text(")"), sep_by(expr(), text(","))),
        )
    };

    let init = || {
        (
            text("let"),
            sep_by1(ident(), text(",")),
            text("="),
            sep_by1(expr(), text(",")),
        )
    };
    let assignment = || attempt((ident(), text("="), expr()));

    let index = || (ident(), between(text("["), text("]"), expr()));

    let ret = || (text("return"), sep_by(expr(), text(",")));

    let wh = || {
        (
            text("while"),
            expr(),
            between(text("{"), text("}"), many(expr())),
        )
    };

    let if_ = || {
        (
            text("if"),
            expr(),
            between(text("{"), text("}"), many(expr())),
            optional(text("else").with(between(text("{"), text("}"), many(expr())))),
        )
    };

    choice((
        ret().map(|(_, b)| Expr::Return(b)),
        assignment().map(|(n, _, b)| Expr::Assign(n, Box::new(b))),
        init().map(|(_, n, _, b)| Expr::Let(n, b)),
        wh().map(|(_, pred, body)| Expr::While(Box::new(pred), body)),
        if_().map(|(_, pred, t_body, f_body)| Expr::If(Box::new(pred), t_body, f_body)),
        // float().map(|x| Expr::Lit(Value::Number(x))),
        literal().map(Expr::Lit),
        attempt(index().map(|(v,i)| Expr::Index(Box::new(Expr::Var(v)),Box::new(i)))),
        attempt(call()).map(|(n, a)| Expr::Call(n, a)),
        ident().map(Expr::Var),
        // arg_list(expr).map(Expr::Tuple),
    ))
}

pub fn function<'a, I>() -> impl Parser<Input = I, Output = TopLevel<'a>>
where
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    struct_parser!(Function {
        _: text("fn"),
        name: ident(),
        args: between(text("("), text(")"), sep_by(ident(), text(","))),
        n_returns: optional(text("->").with(int().map(|x| x as usize))).map(|x| x.unwrap_or(0)),
        body: between(text("{"), text("}"), many(expr())),
    })
    .map(|x| TopLevel::Function(x))
}

pub fn bc_function<'a, I>() -> impl Parser<Input = I, Output = TopLevel<'a>>
where
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    struct_parser!(BcFunction {
        _: text("bc"),
        name: ident(),
        args: between(text("("), text(")"), sep_by(ident(), text(","))),
        body: between(text("{"), text("}"), many(attempt(bytecode::parse_instr()))),
    })
    .map(|x| TopLevel::BcFunction(x))
}

pub fn parse_file<'a, I>() -> impl Parser<Input = I, Output = Vec<TopLevel<'a>>>
where
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(
        skip_whitespace(),
        eof(),
        many(choice((function(), bc_function(), use_parse()))),
    )
}

parser!{
    pub fn expr['a, I]()(I) -> Expr<'a>
    where [
        I: RangeStream<Item = char, Range = &'a str>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        expr_()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn number(x: f64) -> Expr<'static> {
        Expr::Lit(Value::Number(x))
    }

    #[test]
    fn expr_test() {
        let result = expr().easy_parse("1");
        assert_eq!(result, Ok((number(1.), "")));

        let result = expr().easy_parse("print(2)");
        assert_eq!(result, Ok((Expr::Call("print", vec![number(2.)]), "")));

        let result = expr().easy_parse("print(2) ");
        assert_eq!(result, Ok((Expr::Call("print", vec![number(2.)]), "")));

        let result = expr().easy_parse("add(1,2) ");
        assert_eq!(
            result,
            Ok((Expr::Call("add", vec![number(1.), number(2.)]), ""))
        );

        let result = expr().easy_parse("xvar");
        assert_eq!(result, Ok((Expr::Var("xvar"), "")));

        let result = expr().easy_parse("var[var[var[1]]]");
        assert_eq!(result, 
            Ok((
                Expr::Index(
                    Box::new(Expr::Var("var")),
                    Box::new(Expr::Index(
                        Box::new(Expr::Var("var")),
                        Box::new(Expr::Index(
                            Box::new(Expr::Var("var")),
                            Box::new(Expr::Lit(Value::Number(1.0)))
                        ))
                    ))
                ),
                ""
            ))
        );
    }

    #[test]
    fn function_test() {
        let result = parse_file().easy_parse("fn test(a, b) {print(a) print(b)}");
        assert_eq!(
            result,
            Ok((
                vec![TopLevel::Function(Function {
                    name: "test",
                    args: vec!["a", "b"],
                    n_returns: 0,
                    body: vec![
                        Expr::Call("print", vec![Expr::Var("a")]),
                        Expr::Call("print", vec![Expr::Var("b")])
                    ]
                })],
                ""
            )),
        );
    }

    #[test]
    fn bc_function_test() {
        let result = parse_file().easy_parse("bc test(a, b) {add\n}");
        assert_eq!(
            result,
            Ok((
                vec![TopLevel::BcFunction(BcFunction {
                    name: "test",
                    args: vec!["a", "b"],
                    body: vec![bytecode::Instr::new(None, "add", None)]
                })],
                ""
            )),
        );
    }
}
