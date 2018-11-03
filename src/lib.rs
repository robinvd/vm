#![recursion_limit = "1000"]
#![allow(clippy::redundant_closure, clippy::cast_lossless)]

extern crate regex;
#[macro_use]
extern crate combine;

pub mod compiler;
pub mod parser;
pub mod vm;

#[cfg(test)]
mod tests;
