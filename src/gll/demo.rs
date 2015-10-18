#![allow(non_camel_case_types)]

// S ::= A S d | B S | \empty
// A ::= a | c
// B ::= a | b

#[cfg(test)]
fn demo(input: &'static str) -> Result<Success> {
    let g = Graph::new();
    Context::new(input.as_bytes(), &g).main()
}

#[test]
fn demo_1a() {
    // S -> A S d ->* a d
    demo("ad").unwrap();
}
#[test]
fn demo_1b() {
    // S -> A S d -> A A S d d ->* a a d d
    demo("aadd").unwrap();
}
#[test]
fn demo_1c() {
    // S -> A S d -> A A S d d ->* a c d d
    demo("acdd").unwrap();
}
#[test]
fn demo_1d() {
    // S -> A S d -> A A S d d ->* c a d d
    demo("cadd").unwrap();
}

#[test]
fn demo_2a() {
    // S -> B S ->* b
    demo("b").unwrap();
}
#[test]
fn demo_2b() {
    // S -> B S -> B B S ->* b b
    demo("bb").unwrap();
}
#[test]
fn demo_2c() {
    // S -> B S -> B B S -> B B B S ->* b b b
    demo("bbb").unwrap();
}

#[test]
fn demo_3a() {
    // S -> B S -> B A S d ->* b a d
    demo("bad").unwrap();
}
#[test]
fn demo_3b() {
    // S -> B S -> B A S d ->* b c d
    demo("bcd").unwrap();
}
#[test]
fn demo_3c() {
    // S -> B S -> B B S -> B B A S d ->* b b a d
    demo("bbad").unwrap();
}
#[test]
fn demo_3d() {
    // S -> B S -> B B S -> B B A S d ->* b b c d
    demo("bbcd").unwrap();
}
#[test]
fn demo_3e() {
    // S -> A S d -> A B S d ->* a b d
    demo("abd").unwrap();
}
#[test]
fn demo_3f() {
    // S -> A S d -> A B S d ->* c b d
    demo("cbd").unwrap();
}
#[test]
fn demo_3g() {
    // S -> B S -> B A S d -> B A A S d d ->* b a c d d
    demo("bacdd").unwrap();
}
#[test]
fn demo_3h() {
    // S -> B S -> B A S d -> B A A S d d ->* b c a d d
    demo("bcadd").unwrap();
}
#[test]
fn demo_3i() {
    // S -> B S -> B B S -> B B A S d -> B B A A S d d ->* b b c a d d
    demo("bbcadd").unwrap();
}
#[test]
fn demo_3j() {
    // S -> B S -> B B S -> B B A S d -> B B A A S d d ->* b b a c d d
    demo("bbacdd").unwrap();
}
#[test]
fn demo_3k() {
    // S ->* B B A A S d d -> B B A A A S d d d ->* b b a a c d d d
    demo("bbaacddd").unwrap();
}
#[test]
fn demo_3l() {
    // S ->* B B A A S d d -> B B A A A S d d d ->* b b a c c d d d
    demo("bbaccddd").unwrap();
}
#[test]
fn demo_3m() {
    // S ->* B B A A S d d -> B B A A A S d d d ->* b b c a c d d d
    demo("bbcacddd").unwrap();
}

#[test]
fn demo_4a() {
    // S -> B S -> a S -> a
    demo("a").unwrap();
}

#[test]
fn demo_5() {
    demo("").unwrap();
    demo("c").unwrap_err();
    demo("d").unwrap_err();
    demo("abcacddd").unwrap();
}


use pgy_runtime::graph::{Graph, Node};
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
#[derive(Copy, Clone, PartialEq)]
struct GData(Option<(Label, InputPos)>);
impl GData {
    fn dummy() -> GData { GData(None) }
    fn new(l: Label, i: InputPos) -> GData {
        GData(Some((l, i)))
    }
    fn label(&self) -> Label {
        self.0.unwrap().0
    }
}
impl ::std::fmt::Debug for GData {
    fn fmt(&self, w: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self.0 {
            None => {
                write!(w, "$")
            }
            Some(p) => {
                write!(w, "{:?}^{:?}", p.0, (p.1).0)
            }
        }
    }
}
#[derive(Copy, Clone)]
struct Stack<'g>(&'g Node<'g, GData>);
impl<'g> PartialEq for Stack<'g> {
    fn eq(&self, rhs: &Stack<'g>) -> bool {
        (self.0 as *const _) == (rhs.0 as *const _)
    }
}
impl<'g> Stack<'g> {
    fn empty(&self) -> bool {
        self.0.children().count() == 0
    }
}
impl<'g> ::std::fmt::Debug for Stack<'g> {
    fn fmt(&self, w: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(w, "Stack [ ");
        let mut n = self.0;
        loop {
            write!(w, "{:?} ", n.data);
            match n.children().count() {
                0 => break,
                1 => { n = n.children().next().unwrap(); }
                _ => { write!(w, ".."); break; }
            }
        }
        write!(w, "]")
    }
}
#[derive(Copy, Clone)]
struct G<'g> {
    graph: &'g Graph<'g, GData>,
    dummy: &'g Node<'g, GData>,
}
struct Set<T:PartialEq> { elems: Vec<T> }
impl<T:PartialEq> Set<T> {
    fn new() -> Set<T> {
        Set { elems: Vec::new() }
    }
    fn add(&mut self, t: T) {
        if !self.elems.contains(&t) {
            self.elems.push(t);
        }
    }
    fn elems(&self) -> &[T] {
        &self.elems[..]
    }
}
struct R<'g> {
    todo: Vec<Desc<'g>>,
    seen: Vec<Desc<'g>>, // (actually a set)
}
impl<'g> ::std::fmt::Debug for R<'g> {
    fn fmt(&self, w: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(w, "R {{ todo: [");
        let mut seen_one = false;
        for d in &self.todo {
            if seen_one { write!(w, ", "); }
            write!(w, "{:?}", d);
            seen_one = true;
        }
        write!(w, "], seen: {{");
        seen_one = false;
        for d in &self.seen {
            if !self.todo.contains(&d) {
                if seen_one { write!(w, ", "); }
                write!(w, "{:?}", d);
                seen_one = true;
            }
        }
        write!(w, "}} }}")
    }
}

impl<'g> R<'g> {
    fn new() -> R<'g> {
        R { todo: vec![], seen: vec![] }
    }
    fn add(&mut self, d: Desc<'g>) {
        if !self.seen.contains(&d) {
            db!("    R::add d: {:?}", d);
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
    g: G<'g>,
    s: Stack<'g>,
    popped: Set<(Stack<'g>, InputPos)>,
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
    pub fn new(t: &'i [u8], g: &'g Graph<'g, GData>) -> Context<'i, 'g> {
        use self::Label as L;
        let d = g.add_node(GData::dummy());
        let s = g.add_node(GData::new(L::_0, InputPos(0)));
        s.add_child(d);
        Context { i: InputPos(0),
                  I: ts(t),
                  r: R::new(),
                  g: G { graph: g, dummy: d },
                  s: Stack(s),
                  popped: Set::new(),
        }
    }
}

impl<'i, 'g> Context<'i, 'g> {
    fn i_in_core(&self, terms: &[char], end: EndToken) -> bool {
        match (self.I.get(self.i.0), end) {
            (None, EndToken::Incl) => true,
            (None, EndToken::Excl) => false,
            (Some(c), _) => terms.contains(&(c.0 as char)),
        }
    }
    fn i_in_end(&self, terms: &[char]) -> bool { self.i_in_core(terms, EndToken::Incl) }
    fn i_in(&self, terms: &[char]) -> bool { self.i_in_core(terms, EndToken::Excl) }
}

impl<'i, 'g> Context<'i, 'g> {
    fn create(&mut self, l: Label) {
        db!("  create self: {:?} l: {:?}", self, l);
        let L_j = GData::new(l, self.i);
        let u = self.s;
        let v = self.g.graph.nodes()
            .find(|n| n.data == L_j)
            .map(|p|*p);
        let v = match v {
            Some(v) => v,
            None => self.g.graph.add_node(L_j)
        };
        if v.children().find(|c| Stack(c) == u).is_none() {
            v.add_child(u.0);
            for &(p, k) in self.popped.elems() {
                if p == Stack(v) {
                    self.r.add(Desc(l, u, k));
                }
            }
        }
        self.s = Stack(v);
    }

    fn pop(&mut self) {
        db!("  pop self: {:?}", self);
        // pop L off stack s = s'::L, add (L, s', i) to R, where
        // i is current input position.
        let u = self.s;
        let j = self.i;
        if u != Stack(self.g.dummy) {
            self.popped.add((u, j));
            let L_u = u.0.data.label();
            for v in u.0.children() {
                self.add(L_u, Stack(v), j);
            }
        }
    }

    fn add(&mut self, l: Label, u: Stack<'g>, j: InputPos) {
        // N.B. two U_j and R are combined into one (they are separate
        // in the paper); only one operation is needed here.
        self.r.add(Desc(l, u, j));
    }

    fn add_s(&mut self, l: Label) {
        let u = self.s;
        let j = self.i;
        self.add(l, u, j);
    }
}

impl<'i, 'g> Context<'i, 'g> {
    fn main(&mut self) -> Result<Success> {
        use self::Label as L;
        let mut pc = L::S;
        macro_rules! goto {
            ($l:expr) => {
                { db!("goto {:?} to {:?}", pc, $l);
                  pc = $l;
                  continue;
                }
            }
        }

        // The following is a loop for processing the grammar
        //   S ::= A S d | B S | \empty
        //   A ::= a | c
        //   B ::= a | b

        loop {
            pc = match pc {

                // This is kernel of the loop, but it is *not* where
                // we start (note that `pc` is set to `L::S` above).
                L::_0 => {
                    match self.r.pop() {
                        Some(Desc(L, u, j)) => {
                            self.s = u;
                            self.i = j;
                            goto!( L );
                        }
                        None => {
                            if self.r.seen.contains(
                                &Desc(L::_0,
                                      Stack(self.g.dummy),
                                      InputPos(self.I.len()))) {
                                return Ok(Success);
                            } else {
                                return Err(ParseError);
                            }
                        }
                    }
                }

                L::S => { // S ::= A S d | B S | \empty
                    // FIRST(A) = {a,c}
                    if self.i_in(&['a', 'c']) { self.add_s(L::S_1); }
                    // FIRST(B) = {a,b}
                    if self.i_in(&['a', 'b']) { self.add_s(L::S_2); }
                    // FIRST(\empty) = {\empty}; FOLLOW(S) = {d, $}
                    if self.i_in_end(&['d']) { self.add_s(L::S_3); }
                    goto!( L::_0 );
                }

                L::S_1 => { self.create(L::_1); goto!( L::A ); }

                L::_1 => { self.create(L::_2); goto!( L::S ); }

                L::_2 => {
                    if self.i_in(&['d']) {
                        self.i.incr();
                        self.pop();
                    }
                    goto!( L::_0 );
                }

                L::S_2 => { self.create(L::_3); goto!( L::B ); }

                L::_3 => { self.create(L::_4); goto!( L::S ); }

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
