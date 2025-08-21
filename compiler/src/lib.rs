#![allow(dead_code, unused_variables)]
#![feature(assert_matches)]

pub mod ast;
pub mod disjoint_set;
pub mod env;
pub mod error;
pub mod green_node;
pub mod interner;
pub mod lexer;
pub mod name_resolution;
pub mod parser;
pub mod syntax_tree;
pub mod token;
pub mod ty;
pub mod type_ck;
pub mod value;
pub mod visitor;
