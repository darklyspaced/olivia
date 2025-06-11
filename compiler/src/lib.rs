#![allow(dead_code, unused_variables)]
#![feature(assert_matches)]

pub mod ast;
pub mod disjoint_set;
pub mod env;
pub mod error;
pub mod interner;
pub mod lexer;
pub mod parser;
pub mod token;
pub mod ty;
pub mod tyck_core;
pub mod type_ck;
pub mod value;
