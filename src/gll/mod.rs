#![allow(dead_code, unused_must_use, unused_mut, non_snake_case)]

#[derive(Debug)]
pub struct ParseError;
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

pub mod demo {
    #![allow(non_camel_case_types)]

    // S ::= A S d | B S
    // A ::= a | c
    // B ::= a | b

    #[test]
    fn demo_1() {
        let g = Graph::new();
        Context::new("ad".as_bytes(), &g).main().unwrap();
    }

    use graph::gss::{Graph, Node};
    use super::*;
    use super::{T, ts, EndToken};

    #[derive(Copy, Clone, PartialEq, Debug)]
    enum Label {
        S,
        _0,
        S_1,
        _1,
        _2,
        S_2,
        _3,
        _4,
        S_3,
        A,
        B,
    }
    #[derive(Copy, Clone, PartialEq, Debug)]
    struct InputPos(usize);
    impl InputPos {
        fn incr(&mut self) {
            self.0 += 1;
        }
    }
    #[derive(Copy, Clone, PartialEq, Debug)]
    struct Desc<'g>(Label, Stack<'g>, InputPos);
    type GData = (Label, InputPos);
    #[derive(Copy, Clone)]
    struct Stack<'g>(&'g Node<'g, GData>);
    impl<'g> PartialEq for Stack<'g> {
        fn eq(&self, rhs: &Stack<'g>) -> bool {
            (self as *const _) == (rhs as *const _)
        }
    }
    impl<'g> Stack<'g> {
        fn empty(&self) -> bool {
            self.0.child().is_none()
        }
    }
    impl<'g> ::std::fmt::Debug for Stack<'g> {
        fn fmt(&self, w: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
            write!(w, "[");
            let mut n = self.0;
            let mut seen_one = false;
            loop {
                match *n {
                    Node::Dummy => break,
                    Node::Data(ref dn) => {
                        if seen_one { write!(w, ", "); }
                        seen_one = true;
                        write!(w, "{:?}", dn.data);
                        n = dn.child();
                    }
                }
            }
            write!(w, "]")
        }
    }
    type G<'g> = Graph<'g, GData>;
    struct R<'g> {
        todo: Vec<Desc<'g>>,
        seen: Vec<Desc<'g>>, // (actually a set)
    }
    impl<'g> ::std::fmt::Debug for R<'g> {
        fn fmt(&self, w: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
            write!(w, "{{");
            let mut seen_one = false;
            for d in &self.seen {
                if seen_one { write!(w, ", "); }
                if self.todo.contains(&d) {
                    write!(w, "{:?}", d);
                    seen_one = true;
                } else {
                    // write!(w, "({:?})", d);
                }
            }
            write!(w, "}}")
        }
    }

    impl<'g> R<'g> {
        fn new() -> R<'g> {
            R { todo: vec![], seen: vec![] }
        }
        fn add(&mut self, d: Desc<'g>) {
            if !self.seen.contains(&d) {
                self.seen.push(d);
                self.todo.push(d);
            }
        }
        fn nonempty(&self) -> bool {
            self.todo.len() > 0
        }
        fn pop(&mut self) -> Option<Desc<'g>> {
            self.todo.pop()
        }
    }

    struct Context<'i, 'g> {
        i: InputPos,
        I: &'i [T],
        r: R<'g>,
        g: &'g G<'g>,
        s: Stack<'g>,
    }

    impl<'i, 'g> ::std::fmt::Debug for Context<'i, 'g> {
        fn fmt(&self, w: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
            write!(w, "Context {{");
            let (i0, i1) = self.I.split_at(self.i.0);
            write!(w, "I: {:?}({}){:?}, ", T::s(i0), self.i.0, T::s(i1));
            write!(w, "r: {:?}, ", self.r);
            write!(w, "g, ");
            write!(w, "s: {:?}", self.s);
            write!(w, "}}")
        }
    }

    impl<'i, 'g> Context<'i, 'g> {
        pub fn new(t: &'i [u8], g: &'g G<'g>) -> Context<'i, 'g> {
            let dummy = g.add_dummy();
            Context { i: InputPos(0),
                      I: ts(t),
                      r: R::new(),
                      g: g,
                      s: Stack(dummy)
            }
        }
        fn i_in_core(&self, terms: &[char], end: EndToken) -> bool {
            match (self.I.get(self.i.0), end) {
                (None, EndToken::Incl) => true,
                (None, EndToken::Excl) => false,
                (Some(c), _) => terms.contains(&(c.0 as char)),
            }
        }
        fn i_in_end(&self, terms: &[char]) -> bool { self.i_in_core(terms, EndToken::Incl) }
        fn i_in(&self, terms: &[char]) -> bool { self.i_in_core(terms, EndToken::Excl) }

        fn push(&mut self, l: Label) {
            println!("  push self: {:?} l: {:?}", self, l);
            let p = self.g.add_node((l, self.i), self.s.0);
            self.s = Stack(p);
        }

        fn pop(&mut self) {
            println!("  pop self: {:?}", self);
            // pop L off stack s = s'::L, add (L, s', i) to R, where
            // i is current input position.
            match *self.s.0 {
                Node::Dummy => {},
                Node::Data(ref dn) => {
                    let d = &dn.data;
                    let s_prime = Stack(dn.child());
                    let desc = Desc(d.0, s_prime, self.i);
                    self.r.add(desc);
                    self.s = s_prime;
                }
            }
        }

        fn main(&mut self) -> Result<Success> {
            use self::Label as L;
            let mut pc = L::S;
            macro_rules! goto {
                ($l:expr) => {
                    { println!("goto {:?} to {:?}", pc, $l);
                      pc = $l;
                      continue;
                    }
                }
            }
            loop {
                pc = match pc {
                    L::S => {
                        if self.i_in(&['a', 'c']) { self.r.add(Desc(L::S_1, self.s, self.i)); }
                        if self.i_in(&['a', 'b']) { self.r.add(Desc(L::S_2, self.s, self.i)); }
                        if self.i_in_end(&['d']) { self.r.add(Desc(L::S_3, self.s, self.i)); }
                        L::_0
                    }

                    L::_0 => {
                        match self.r.pop() {
                            Some(Desc(L, s_1, j)) => {
                                if L == L::_0 && s_1.empty() && j.0 == self.I.len() {
                                    return Ok(Success);
                                } else {
                                    self.s = s_1;
                                    self.i = j;
                                    goto!( L )
                                }
                            }
                            None =>
                                return Err(ParseError),
                        }
                    }
                    L::S_1 => { self.push(L::_1); goto!( L::A ); }

                    L::_1 => { self.push(L::_2); goto!( L::S ); }

                    L::_2 => {
                        if self.i_in(&['d']) {
                            self.i.incr();
                            self.pop();
                        }
                        goto!( L::_0 );
                    }

                    L::S_2 => { self.push(L::_3); goto!( L::B ); }

                    L::_3 => { self.push(L::_4); goto!( L::_0 ); }

                    L::_4 => { self.pop(); goto!( L::_0 ); }

                    L::S_3 => { self.pop(); goto!( L::_0 ); }

                    L::A => {
                        if self.i_in(&['a']) {
                            self.i.incr();
                            self.pop();
                            goto!( L::_0 );
                        } else {
                            if self.i_in(&['c']) {
                                self.i.incr();
                                self.pop();
                            }
                            goto!( L::_0 );
                        }
                    }
                    L::B => {
                        if self.i_in(&['a']) {
                            self.i.incr();
                            self.pop();
                            goto!( L::_0 );
                        } else {
                            if self.i_in(&['b']) {
                                self.i.incr();
                                self.pop();
                            }
                            goto!( L::_0 );
                        }
                    }
                }
            }
        }
    }
}

pub mod demo_section_2 {
    // S ::= A S d | B S
    // A ::= a | c
    // B ::= a | b

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
}
