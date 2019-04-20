use lalrpop_util::lalrpop_mod;

// pub mod code;
mod ast;
lalrpop_mod!(pub grammar, "/parser/grammar.rs");

pub use ast::*;

fn is_space(x: char) -> bool {
    [' ', '\t', '\0'].contains(&x)
}
