use gll::codegen::Backend;
use grammar::{Grammar, NontermName, TermName, Sym};

use std::borrow::Cow;
use std::mem;

pub enum Command {
    One(String),
    Seq(Vec<Command>),
    If(Expr, Box<Command>),
    IfElse(Expr, Box<Command>, Box<Command>)
}

pub enum CommandSeq { Just(Command), Rev(Vec<Command>) }

impl Command {
    fn seq(self) -> CommandSeq {
        if let Command::Seq(v) = self {
            CommandSeq::Rev(v)
        } else {
            CommandSeq::Just(self)
        }
    }
}

impl Iterator for CommandSeq {
    type Item = Command;
    fn next(&mut self) -> Option<Command> {
        let cmd = match *self {
            CommandSeq::Just(ref mut c) =>
                mem::replace(c, Command::Seq(vec![])),
            CommandSeq::Rev(ref mut v) =>
                match v.pop() {
                    None => return None,
                    Some(cmd) => cmd,
                },
        };
        if let &mut CommandSeq::Just(..) = self {
            mem::replace(self, CommandSeq::Rev(vec![]));
        }
        Some(cmd)
    }
}

pub struct Expr(String);

#[derive(Clone)]
pub struct Label(Cow<'static, str>);
pub struct Block(Label, Command);

struct RustBackend<'a>(&'a Grammar);

impl<'a> Backend<'a> for RustBackend<'a> {
    type Command = Command;
    type Expr = Expr;
    type Label = Label;
    type Block = Block;

    fn new(g: &'a Grammar) -> Self { RustBackend(g) }

    fn label_0(&self) -> Label { Label("L::_0".into()) }

    // R_A_k labels function call return lines.
    fn return_label(&self, (a, k): (NontermName, usize)) -> Self::Label {
        Label(format!("L::R_{}_{}", a, k).into())
    }

    // L_A labels parse function for A.
    fn nonterm_label(&self, a: NontermName) -> Self::Label {
        Label(format!("L::L_{}", a).into())
    }

    // L_A_i labels function for parsing ith alternate α_i of A.
    fn alternate_label(&self,
                       (a, i): (NontermName, usize)) -> Self::Label {
        Label(format!("L::A_{}_{}", a, i).into())
    }

    // `L: C`
    // (note that `C` must have control flow ending in goto...)
    fn block(&mut self, l: Self::Label, c: Self::Command) -> Self::Block {
        Block(l, c)
    }

    // Execute this command to report the parse attempt failed.
    fn report_parse_failure(&mut self, msg: &str) -> Self::Command {
        Command::One(format!("return Err(({});", msg))
    }

    // Execute this command if something unexpected happened
    // in the generated code.
    fn panic_fail(&mut self, msg: &str) -> Self::Command {
        Command::One(format!("panic!({})", msg))
    }

    // the no-op command makes some constructions easier.
    fn no_op(&mut self) -> Self::Command {
        Command::Seq(vec![])
    }

    // `cmd1, cmd2`
    fn seq(&mut self,
           cmd1: Self::Command,
           cmd2: Self::Command) -> Self::Command {
        Command::Seq(cmd1.seq()
                     .chain(cmd2.seq()).collect())
    }

    // `if test { then }
    fn if_(&mut self,
           test: Self::Expr,
           then: Self::Command) -> Self::Command {
        Command::If(test, Box::new(then))
        
    }

    // `if test { then } else { else_ }`
    fn if_else(&mut self,
               test: Self::Expr,
               then: Self::Command,
               else_: Self::Command) -> Self::Command {
        Command::IfElse(test, Box::new(then), Box::new(else_))
    }

    // `j := j + 1`
    fn increment_curr(&mut self) -> Self::Command {
        Command::One("self.i.incr();".into())
    }

    // let L = label;
    // `goto L`
    fn goto(&mut self, label: Self::Label) -> Self::Command {
        Command::One(format!("goto!( {} )", label.0))
    }

    // `I[j] == a`
    fn curr_matches_term(&mut self, a: TermName) -> Self::Expr {
        Expr(format!("self.i_in(&['{}'])", a))
    }

    // let x = I[j]; let α = alpha;
    // `x in FIRST(α) or empty in FIRST(α) and x in FOLLOW(A)`
    //
    // The leading optional component in alpha is meant to be
    // the first element of alpha, if it is present at all.
    fn test<E:Copy>(
        &mut self,
        a: NontermName,
        alpha: (Option<NontermName>, &[Sym<E>])) -> Self::Expr {

        // FIXME: copying is slow; change interface of either
        // `fn test` or of `fn first`.
        let (opt_one, rest) = alpha;
        let mut alpha = Vec::with_capacity(
            rest.len() + if opt_one.is_some() { 1 } else { 0 });
        if let Some(n) = opt_one {
            // the extra doesn't matter for `first`
            alpha.push(Sym::N { name: n, x: () });
        }
        alpha.extend(rest.iter().map(|s| s.drop_x()));

        let first = self.0.first(&alpha);
        let first_terms: String = first.terms().iter()
            .map(|t| format!("{},", t)).collect();
        if first.is_nullable() {
            let follow = self.0.follow(a);
            let follow_terms: String = follow.terms().iter()
                .map(|t|format!("{},", t)).collect();
            // FIXME: should first do set-union between first_terms
            // and follow_terms on this branch, avoiding potential
            // duplication of effort below.
            Expr(format!("(self.i_in(&[{}]) || self.i_in(&[{}]))",
                         first_terms, follow_terms))
        } else {
            Expr(format!("self.i_in(&[{}])", first_terms))
        }
    }

    // `c_u := create(l, c_u, j)`
    fn create(&mut self,
              l: Self::Label) -> Self::Command {
        Command::One(format!("self.create({});", l.0))
    }

    // `add(l, c_u, j)
    fn add(&mut self, l: Self::Label) -> Self::Command {
        Command::One(format!("self.add_s({})", l.0))
    }

    // `pop(c_u, j)`
    fn pop(&mut self) -> Self::Command {
        Command::One(format!("self.pop()"))
    }
}
