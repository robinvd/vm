use combine::range::{take_while, take_while1, recognize};
use combine::char::{letter, string};
use combine::{
    choice, eof, many, many1, one_of, optional, r#try, token, ParseError, Parser,
    RangeStream, Stream, satisfy, between, sep_by, attempt
};

use crate::parser::*;

#[derive(Debug, PartialEq)]
pub struct Function<'a> {
    pub name: &'a str,
    pub args: Vec<&'a str>,
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
}

#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    Lit(isize),
    Call(&'a str, Vec<Expr<'a>>),
    Var(&'a str),
    While(Box<Expr<'a>>, Vec<Expr<'a>>),
    Assignment(&'a str, Box<Expr<'a>>),
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
    let parser = (
        letter(),
        take_while(|c: char| c.is_alphanumeric()),
    );
    recognize(parser).skip(skip_whitespace())
}

parser!{
    pub fn text['a, I](s: &'static str)(I) -> &'a()
    where [
        I: RangeStream<Item = char, Range = &'a str>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        string(s).skip(skip_whitespace()).map(|_| &())
    }
}

fn expr_<'a, I>() -> impl Parser<Input = I, Output = Expr<'a>>
where
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let int = || {
        take_while1(|c: char| is_num(c))
            .skip(skip_whitespace())
            .map(|s: &str| s.parse::<isize>().unwrap())
    };

    let call = || {
        (ident(), between(text("("), text(")"), sep_by(expr(), text(","))))
    };

    let assignment = || {
        (text("let"), ident(), text("="), expr())
    };

    let wh = || {
        (
            text("while"),
            expr(),
            between(text("{"), text("}"), many(expr()))
        )
    };

    choice((
        assignment().map(|(_, n, _, b)| Expr::Assignment(n, Box::new(b))),
        wh().map(|(_, pred, body)| Expr::While(Box::new(pred), body)),
        int().map(|x| Expr::Lit(x)),
        attempt(call()).map(|(n, a)| Expr::Call(n,a)),
        ident().map(|n| Expr::Var(n)),
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
        body: between(text("{"), text("}"), many(expr())),
    }).map(|x| TopLevel::Function(x))
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
    }).map(|x| TopLevel::BcFunction(x))
}

pub fn parse_file<'a, I>() -> impl Parser<Input = I, Output = Vec<TopLevel<'a>>>
where
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(
        skip_whitespace(),
        eof(),
        many(choice((function(), bc_function()))),
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
    #[test]
    fn expr_test() {
        let result = expr().easy_parse("1");
        assert_eq!(result, Ok((Expr::Lit(1), "")));

        let result = expr().easy_parse("print(2)");
        assert_eq!(result, Ok((Expr::Call("print", vec![Expr::Lit(2)]), "")));

        let result = expr().easy_parse("print(2) ");
        assert_eq!(result, Ok((Expr::Call("print", vec![Expr::Lit(2)]), "")));

        let result = expr().easy_parse("add(1,2) ");
        assert_eq!(result, Ok((Expr::Call("add", vec![Expr::Lit(1),Expr::Lit(2)]), "")));

        let result = expr().easy_parse("xvar");
        assert_eq!(result, Ok((Expr::Var("xvar"), "")));
    }

    #[test]
    fn function_test() {
        let result = parse_file().easy_parse("fn test(a, b) {print(a) print(b)}");
        assert_eq!(
            result,
            Ok((vec![
                TopLevel::Function(Function{
                name: "test",
                args: vec!["a", "b"],
                body: vec![
                    Expr::Call("print", vec![Expr::Var("a")]),
                    Expr::Call("print", vec![Expr::Var("b")])
                ]
            })], "")),
        );
    }

    #[test]
    fn bc_function_test() {
        let result = parse_file().easy_parse("bc test(a, b) {add\n}");
        assert_eq!(
            result,
            Ok((vec![
                TopLevel::BcFunction(BcFunction{
                name: "test",
                args: vec!["a", "b"],
                body: vec![
                    bytecode::Instr::new(None, "add", None)
                ]
            })], "")),
        );
    }
}