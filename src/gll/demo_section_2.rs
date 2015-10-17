// S ::= A S d | B S
// A ::= a | c
// B ::= a | b

#[cfg(test)]
fn demo(input: &'static str) -> Result<Success> {
    Context::new(input.as_bytes()).main()
}

#[test]
fn demo_1() {
    demo("ad").unwrap();
    demo("aadd").unwrap();
    demo("acdd").unwrap();
    demo("cadd").unwrap();
}

#[test]
fn demo_2() {
    demo("b").unwrap();
    demo("bb").unwrap();
    demo("bbb").unwrap();
}

#[test]
fn demo_3() {
    demo("bad").unwrap();
    demo("bcd").unwrap();
    demo("bbad").unwrap();
    demo("bbcd").unwrap();
    demo("abd").unwrap();
    demo("cbd").unwrap();
    demo("bacdd").unwrap();
    demo("bcadd").unwrap();
    demo("bbcadd").unwrap();
    demo("bbacdd").unwrap();
    demo("bbaacddd").unwrap();
    demo("bbaccddd").unwrap();
    demo("bbcacddd").unwrap();
}

#[cfg(ignored)] // this one, exercising the first alt of B, doesn't work yet.
#[test]
fn demo_4() {
    demo("a").unwrap();
}

use super::*;
use super::{T, ts, EndToken};

#[allow(non_snake_case)]
pub struct Context<'a> {
    i: usize,
    I: &'a [T],
}

impl<'a> Context<'a> {
    pub fn new(t: &'a [u8]) -> Context { Context { i: 0, I: ts(t) } }
    fn i_in_core(&self, terms: &[char], end: EndToken) -> bool {
        match (self.I.get(self.i), end) {
            (None, EndToken::Incl) => true,
            (None, EndToken::Excl) => false,
            (Some(c), _) => terms.contains(&(c.0 as char)),
        }
    }
    fn i_in_end(&self, terms: &[char]) -> bool { self.i_in_core(terms, EndToken::Incl) }
    fn i_in(&self, terms: &[char]) -> bool { self.i_in_core(terms, EndToken::Excl) }

    pub fn main(&mut self) -> Result<Success> {
        self.i = 0;
        if self.i_in_end(&['a', 'b', 'c', 'd']) {
            try!(self.p_s());
        } else {
            return Err(ParseError);
        }
        if self.i_in_end(&[]) {
            Ok(Success)
        } else {
            Err(ParseError)
        }
    }

    fn p_s(&mut self) -> Result<()> {
        if self.i_in(&['a', 'c']) {
            try!(self.p_a());
            try!(self.p_s());
            if self.i_in(&['d']) {
                self.i += 1;
            } else {
                return Err(ParseError);
            }
        } else {
            if self.i_in(&['a', 'b']) {
                try!(self.p_b());
                try!(self.p_s());
            }
        }
        Ok(())
    }

    fn p_a(&mut self) -> Result<()> {
        if self.i_in(&['a']) {
            self.i += 1;
        } else if self.i_in(&['c']) {
            self.i += 1;
        } else {
            return Err(ParseError);
        }
        Ok(())
    }

    fn p_b(&mut self) -> Result<()> {
        if self.i_in(&['a']) {
            self.i += 1;
        } else if self.i_in(&['b']) {
            self.i += 1;
        } else {
            return Err(ParseError);
        }
        Ok(())
    }
}

#[test]
fn demo_papers_grammar() {
}
