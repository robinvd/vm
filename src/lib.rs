#![recursion_limit = "1000"] 

extern crate regex;
#[macro_use]
extern crate combine;

pub mod vm;
pub mod compiler;
pub mod parser;

#[cfg(test)]
mod tests;