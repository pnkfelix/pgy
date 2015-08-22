pub mod demo {
    #![allow(non_camel_case_types)]

    use std::result::Result as StdResult;

    // S ::= A S d | B S
    // A ::= a | c
    // B ::= a | b

    #[derive(PartialEq)]
    struct T(u8);

    fn ts(t: &[u8]) -> &[T] {
        use std::mem;
        unsafe { mem::transmute(t) }
    }

    #[allow(non_snake_case)]
    pub struct Context<'a> {
        i: usize,
        I: &'a [T],
    }

    #[test]
    fn demo_1() {
        Context::new("ad".as_bytes()).main().unwrap();
        Context::new("aadd".as_bytes()).main().unwrap();
        Context::new("acdd".as_bytes()).main().unwrap();
        Context::new("cadd".as_bytes()).main().unwrap();
    }

    #[test]
    fn demo_2() {
        Context::new("b".as_bytes()).main().unwrap();
        Context::new("bb".as_bytes()).main().unwrap();
        Context::new("bbb".as_bytes()).main().unwrap();
    }

    #[test]
    fn demo_3() {
        Context::new("bad".as_bytes()).main().unwrap();
        Context::new("bcd".as_bytes()).main().unwrap();
        Context::new("bbad".as_bytes()).main().unwrap();
        Context::new("bbcd".as_bytes()).main().unwrap();
        Context::new("abd".as_bytes()).main().unwrap();
        Context::new("cbd".as_bytes()).main().unwrap();
        Context::new("bacdd".as_bytes()).main().unwrap();
        Context::new("bcadd".as_bytes()).main().unwrap();
        Context::new("bbcadd".as_bytes()).main().unwrap();
        Context::new("bbacdd".as_bytes()).main().unwrap();
        Context::new("bbaacddd".as_bytes()).main().unwrap();
        Context::new("bbaccddd".as_bytes()).main().unwrap();
        Context::new("bbcacddd".as_bytes()).main().unwrap();
    }

    #[cfg(ignored)] // this one, exercising the first alt of B, doesn't work yet.
    #[test]
    fn demo_4() {
        Context::new("a".as_bytes()).main().unwrap();
    }

    #[derive(Debug)]
    pub struct ParseError;
    pub struct Success;

    pub type Result<T> = StdResult<T, ParseError>;
    enum EndToken { Incl, Excl }
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
}
