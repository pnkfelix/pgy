#![allow(dead_code, unused_must_use, unused_mut, non_snake_case)]

macro_rules! db {
    ($($tt:expr),*) => {
        // println!($($tt),*)
    }
}

#[derive(Debug)]
pub struct ParseError;
#[derive(Debug)]
pub struct Success;
pub use std::result::Result as StdResult;
pub type Result<T> = StdResult<T, ParseError>;

enum EndToken { Incl, Excl }

#[derive(PartialEq)]
struct T(u8);

impl T {
    fn s(ts: &[T]) -> &str {
        use std::mem;
        let ts: &[u8] = unsafe { mem::transmute(ts) };
        ::std::str::from_utf8(ts).unwrap()
    }
}

fn ts(t: &[u8]) -> &[T] {
    use std::mem;
    unsafe { mem::transmute(t) }
}

#[cfg(test)]
pub mod demo;

#[cfg(test)]
pub mod demo_section_2;

pub mod codegen;

#[cfg(test)]
pub mod demo_codegen;
