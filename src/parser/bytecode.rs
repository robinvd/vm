use combine::range::{take_while, take_while1};
use combine::{
    attempt, choice, eof, optional, token, ParseError, Parser, RangeStream,
};

use super::*;

#[derive(Debug, PartialEq)]
pub enum Arg<'a> {
    Text(&'a str),
    Int(isize),
}

#[derive(Debug, PartialEq)]
pub struct Instr<'a> {
    pub label: Option<&'a str>,
    pub instr: &'a str,
    pub arg: Option<Arg<'a>>,
}

impl<'a> Instr<'a> {
    pub fn new(label: Option<&'a str>, instr: &'a str, arg: Option<Arg<'a>>) -> Self {
        Self { label, instr, arg }
    }
}

pub fn parse_instr<'a, I>() -> impl Parser<Input = I, Output = Instr<'a>>
where
    I: RangeStream<Item = char, Range = &'a str>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    // A parser which skips past whitespace.
    // Since we aren't interested in knowing that our expression parser
    // could have accepted additional whitespace between the tokens we also silence the error.
    // let skip_spaces = || spaces().silent();
    // let word = many1(letter()).skip(skip_spaces());
    let skip_space = || take_while(|c| is_space(c));
    let word = || take_while1(|c| is_char(c)).skip(skip_space());
    let int = || {
        take_while1(|c| is_num(c))
            .skip(skip_space())
            .map(|s: &str| s.parse::<isize>().unwrap())
    };
    let single = |c: char| token(c).skip(skip_space());

    let argument = choice((word().map(|t| Arg::Text(t)), int().map(|i| Arg::Int(i))));

    struct_parser!(Instr {
        label: optional(attempt(word().skip(single(':')))),
        instr: word(),
        arg: optional(attempt(argument)),
        _: choice((token('\n').skip(skip_space()).map(|_| ()), eof())),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn instr_test_basic() {
        assert_eq!(
            parse_instr().easy_parse("start: jmp start"),
            Ok((
                Instr::new(Some("start"), "jmp", Some(Arg::Text("start"))),
                ""
            )),
        );
        assert_eq!(
            parse_instr().easy_parse("pop"),
            Ok((Instr::new(None, "pop", None), "")),
        );
        assert_eq!(
            parse_instr().easy_parse("const 1\n"),
            Ok((Instr::new(None, "const", Some(Arg::Int(1))), "")),
        );
    }
}
