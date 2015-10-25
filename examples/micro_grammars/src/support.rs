macro_rules! db {
    ($($tt:expr),*) => {
        println!($($tt),*)
    }
}

use pgy_runtime::*;
use pgy_runtime::graph::{Graph, Node};

enum EndToken { Incl, Excl }

#[derive(PartialEq, Debug)]
struct T(u8);

impl FromChar for T {
    fn from_char(c: char) -> T {
        let u = c as u32;
        if u <= ::std::u8::MAX as u32 {
            T(u as u8)
        } else {
            panic!("input {} out of range", c);
        }
    }
}

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

#[derive(Copy, Clone)]
struct G<'g, LABEL:'g> {
    graph: &'g Graph<'g, GData<LABEL>>,
    dummy: &'g Node<'g, GData<LABEL>>,
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

struct R<'g, LABEL:'g> {
    todo: Vec<Desc<'g, LABEL>>,
    seen: Vec<Desc<'g, LABEL>>, // (actually a set)
}

impl<'g, LABEL: Eq + ::std::fmt::Debug> ::std::fmt::Debug for R<'g, LABEL> {
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

impl<'g, LABEL:'g + Eq + Copy + ::std::fmt::Debug> R<'g, LABEL> {
    fn new() -> R<'g, LABEL> {
        R { todo: vec![], seen: vec![] }
    }
    fn add(&mut self, d: Desc<'g, LABEL>) {
        if !self.seen.contains(&d) {
            db!("    R::add d: {:?}", d);
            self.seen.push(d);
            self.todo.push(d);
        }
    }
    fn nonempty(&self) -> bool {
        self.todo.len() > 0
    }
    fn pop(&mut self) -> Option<Desc<'g, LABEL>> {
        self.todo.pop()
    }
}


pub struct DemoContext<'i, 'g, LABEL:'g> {
    i: InputPos,
    I: &'i [T],
    r: R<'g, LABEL>,
    g: G<'g, LABEL>,
    s: Stack<'g, LABEL>,
    popped: Set<(Stack<'g, LABEL>, InputPos)>,
}

impl<'i, 'g, LABEL:LabelZero + 'g + Copy + Eq + ::std::fmt::Debug> DemoContext<'i, 'g, LABEL> {
    pub fn new(t: &'i [u8], g: &'g Graph<'g, GData<LABEL>>) -> Self {
        let d = g.add_node(GData::dummy());
        let s = g.add_node(GData::new(LABEL::label_zero(), InputPos(0)));
        s.add_child(d);
        DemoContext { i: InputPos(0),
                      I: ts(t),
                      r: R::new(),
                      g: G { graph: g, dummy: d },
                      s: Stack(s),
                      popped: Set::new(),
        }
    }
}

impl<'i, 'g, LABEL: Eq + ::std::fmt::Debug> ::std::fmt::Debug for DemoContext<'i, 'g, LABEL> {
    fn fmt(&self, w: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(w, "DemoContext {{");
        let (i0, i1) = self.I.split_at(self.i.0);
        write!(w, "I: {:?}({}){:?}, ", T::s(i0), self.i.0, T::s(i1));
        write!(w, "r: {:?}, ", self.r);
        write!(w, "g, ");
        write!(w, "s: {:?}", self.s);
        write!(w, "}}")
    }
}

impl<'i, 'g, LABEL:'g + Eq + Copy + ::std::fmt::Debug> DemoContext<'i, 'g, LABEL> {
    fn i_in_core(&self, terms: &[char], end: EndToken) -> bool {
        match (self.I.get(self.i.0), end) {
            (None, EndToken::Incl) => true,
            (None, EndToken::Excl) => false,
            (Some(c), _) => terms.contains(&(c.0 as char)),
        }
    }
    fn i_in_end(&self, terms: &[char]) -> bool { self.i_in_core(terms, EndToken::Incl) }

    fn add(&mut self, l: LABEL, u: Stack<'g, LABEL>, j: InputPos) {
        // N.B. two U_j and R are combined into one (they are separate
        // in the paper); only one operation is needed here.
        self.r.add(Desc(l, u, j));
    }
}



#[derive(Default, Debug)]
pub struct Success;

#[derive(Default, Debug)]
pub struct ParseError;

impl From<&'static str> for ParseError {
    fn from(s: &'static str) -> Self { ParseError }
}

impl<'i, 'g, LABEL: Copy + Eq + ::std::fmt::Debug> Context<'g, LABEL> for DemoContext<'i, 'g, LABEL> {
    type Success = Success;
    type ParseError = ParseError;
    type Term = char;

    fn create(&mut self, l: LABEL) {
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

    fn i_in(&self, terms: &[char]) -> bool { self.i_in_core(terms, EndToken::Excl) }
    fn i_in_end(&self, terms: &[char]) -> bool { self.i_in_core(terms, EndToken::Incl) }
    fn i_len(&self) -> usize { self.I.len() }
    fn i_incr(&mut self) { self.i.incr() }

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

    fn g_dummy(&self) -> &'g Node<'g, GData<LABEL>> { self.g.dummy }
    fn r_pop(&mut self) -> Option<Desc<'g, LABEL>> { self.r.pop() }
    fn r_seen_contains(&self, d: &Desc<'g, LABEL>) -> bool { self.r.seen.contains(d) }
    fn set_s(&mut self, u: Stack<'g, LABEL>) { self.s = u }
    fn set_i(&mut self, j: InputPos) { self.i = j; }

    fn add_s(&mut self, l: LABEL) {
        let u = self.s;
        let j = self.i;
        self.add(l, u, j);
    }
}
