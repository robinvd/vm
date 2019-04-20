use std::ops::Deref;

use codespan::ByteSpan;

use crate::vm::value::{Object, Value};

#[derive(Debug, PartialEq)]
pub struct Function<'a> {
    pub name: &'a str,
    pub args: Vec<&'a str>,
    pub n_returns: usize,
    pub body: Vec<Info<Expr<'a>>>,
    pub span: ByteSpan,
}

#[derive(Debug, PartialEq)]
pub enum TopLevel<'a> {
    Function(Function<'a>),
    // Use(UseTree<'a>),
    Use(Vec<&'a str>),
}

pub type Expr<'a> = Info<InnerExpr<'a>>;

#[derive(Debug, PartialEq)]
pub enum InnerExpr<'a> {
    Lit(Lit<'a>),
    Call(&'a str, Vec<Expr<'a>>),
    Tuple(Vec<Expr<'a>>),
    Var(&'a str),
    While(Box<Expr<'a>>, Vec<Expr<'a>>),
    If(Box<Expr<'a>>, Vec<Expr<'a>>, Option<Vec<Expr<'a>>>),
    Let(&'a str, Box<Expr<'a>>),
    Assign(&'a str, Box<Expr<'a>>),
    Return(Vec<Expr<'a>>),
    Index(Box<Expr<'a>>, Box<Expr<'a>>),
}

#[derive(Debug, PartialEq)]
pub enum Lit<'a> {
    Number(f64),
    Nil,
    True,
    False,
    String(&'a str),
    Dict(Vec<(Expr<'a>, Expr<'a>)>),
    List(Vec<Expr<'a>>),

}

#[derive(Debug, PartialEq)]
pub struct Info<T> {
    pub val: T,
    pub span: ByteSpan,
}

impl<T> Deref for Info<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.val
    }
}