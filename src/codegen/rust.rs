use gll::codegen::{Backend, BackendText, RenderIndent};
use grammar::{Grammar, NontermName, Rule, SetUpdate, Sym, TermName};

use std::borrow::Cow;
use std::iter;
use std::mem;

#[derive(Debug)]
pub enum Command {
    One(String),
    Seq(Vec<Command>),
    If(Expr, Box<Command>),
    IfElse(Expr, Box<Command>, Box<Command>)
}

fn make_indent(i: usize) -> String { iter::repeat(' ').take(i).collect() }

impl RenderIndent for Command {
    fn render_indent(&self, i: usize) -> String {
        let indent = || make_indent(i);
        match *self {
            Command::One(ref s) => indent() + s + "\n",
            Command::Seq(ref cmds) => cmds.iter()
                .map(|cmd| cmd.render_indent(i))
                .collect(),
            Command::If(ref e, ref t) =>
                format!("{}if {} {{\n{}{}}}\n",
                        indent(), e.0,
                        t.render_indent(i + 4),
                        indent()),
            Command::IfElse(ref e, ref tn, ref el) =>
                format!("{}if {} {{\n{}{}}} else {{\n{}{}}}\n",
                        indent(), e.0,
                        tn.render_indent(i + 4),
                        indent(),
                        el.render_indent(i + 4),
                        indent()),
        }
    }
}

pub enum CommandSeq { Just(Command), Rev(Vec<Command>) }

impl Command {
    fn seq(self) -> CommandSeq {
        if let Command::Seq(v) = self {
            CommandSeq::Rev(v.into_iter().rev().collect())
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

#[derive(Debug)]
pub struct Expr(String);

#[derive(Clone, Debug)]
pub struct Label { name: Cow<'static, str> }

#[allow(non_snake_case)]
pub fn Label<N:Into<Cow<'static, str>>>(name: N) -> Label { Label { name: name.into() } }

impl Label {
    fn render_use(&self) -> String { format!("L::{}", self.name) }
    fn render_def(&self) -> String { format!("{}", self.name) }
}

#[derive(Debug)]
pub struct Block(Label, Command);

impl RenderIndent for Block {
    fn render_indent(&self, i: usize) -> String {
        let indent = || make_indent(i);
        format!("{}{} => {{\n{}{}}}\n", indent(), (self.0).render_use(),
                self.1.render_indent(i+4),
                indent())
    }
}

pub type NonTerm = &'static str;

pub struct RustBackend<'a>(pub &'a Grammar<usize>);

impl<'a> RustBackend<'a> {
    pub fn all_labels(&self) -> Vec<Label> { all_labels(self) }
    pub fn all_nonterms(&self) -> Vec<NonTerm> { all_nonterms(self) }
    pub fn impl_for_nonterm<E:Copy>(&self, n: NonTerm, g: &Grammar<E>) -> String {
        impl_for_nonterm(n, g)
    }
    pub fn new(g: &'a Grammar<usize>) -> RustBackend {
        RustBackend(g)
    }
}

impl<'a> BackendText for RustBackend<'a> {
    fn prefix(&self) -> String { prefix(self) }
    fn suffix(&self) -> String { suffix(self) }
    fn rule_indent_preference(&self) -> usize { "            ".len() }
}

const CONTEXT: &'static str = "context";

impl<'a> Backend for RustBackend<'a> {
    type Command = Command;
    type Expr = Expr;
    type Label = Label;
    type Block = Block;

    fn grammar(&self) -> &Grammar<usize> { &self.0 }

    fn label_0(&self) -> Label { Label("_0") }

    // R_N_A_k labels function call return to N from call associated
    // with A_k.
    fn return_label(&self, n: NontermName, (a, k): (NontermName, usize)) -> Self::Label {
        Label(format!("R_{}_{}_{}", n, a, k+1))
    }

    // L_A labels parse function for A.
    fn nonterm_label(&self, a: NontermName) -> Self::Label {
        Label(format!("L_{}", a))
    }

    // L_A_i labels function for parsing ith alternate α_i of A.
    fn alternate_label(&self,
                       (a, i): (NontermName, usize)) -> Self::Label {
        Label(format!("A_{}_{}", a, i+1))
    }

    // `L: C`
    // (note that `C` must have control flow ending in goto...)
    fn block(&self, l: Self::Label, c: Self::Command) -> Self::Block {
        Block(l, c)
    }

    // Execute this command to report the parse attempt failed.
    fn report_parse_failure(&self, msg: &str) -> Self::Command {
        Command::One(format!("return Err(From::from(\"{}\"));", msg))
    }

    // Execute this command if something unexpected happened
    // in the generated code.
    fn panic_fail(&self, msg: &str) -> Self::Command {
        Command::One(format!("panic!(\"{}\");", msg))
    }

    // the no-op command makes some constructions easier.
    fn no_op(&self) -> Self::Command {
        Command::Seq(vec![])
    }

    // `cmd1, cmd2`
    fn seq(&self,
           cmd1: Self::Command,
           cmd2: Self::Command) -> Self::Command {
        Command::Seq(cmd1.seq()
                     .chain(cmd2.seq()).collect())
    }

    // `if test { then }
    fn if_(&self,
           test: Self::Expr,
           then: Self::Command) -> Self::Command {
        Command::If(test, Box::new(then))
    }

    // `if test { then } else { else_ }`
    fn if_else(&self,
               test: Self::Expr,
               then: Self::Command,
               else_: Self::Command) -> Self::Command {
        Command::IfElse(test, Box::new(then), Box::new(else_))
    }

    // `j := j + 1`
    fn increment_curr(&self) -> Self::Command {
        Command::One(format!("{}.i_incr();", CONTEXT).into())
    }

    // let L = label;
    // `goto L`
    fn goto(&self, label: Self::Label) -> Self::Command {
        Command::One(format!("goto!( {} )", label.render_use()))
    }

    // `I[j] == a`
    fn curr_matches_term(&self, a: TermName) -> Self::Expr {
        Expr(format!("{}.i_in(&[C::Term::from_char('{}')])", CONTEXT, a))
    }

    // let x = I[j]; let N = n;
    // `x in FIRST(N$)`
    //
    // The leading optional component in alpha is meant to be
    // the first element of alpha, if it is present at all.
    fn test_end<E:Copy>(&self, n: NontermName) -> Self::Expr {
        let alpha = vec![Sym::N { name: n, x: () }];
        let first = self.0.first(&alpha);
        let first_pred = if first.is_nullable() {
            format!("{}.i_in_end", CONTEXT)
        } else {
            format!("{}.i_in", CONTEXT)
        };
        let term_commas: String =
            first.into_terms().into_iter()
            .map(|term| format!("{},", term))
            .collect();
        Expr(format!("{}(&[{}])", first_pred, term_commas))
    }

    // let x = I[j]; let α = alpha;
    // `x in FIRST(α) or empty in FIRST(α) and x in FOLLOW(A)`
    //
    // The leading optional component in alpha is meant to be
    // the first element of alpha, if it is present at all.
    fn test<E:Copy>(
        &self,
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
        if first.is_nullable() {
            let follow = self.0.follow(a);

            let follow_pred = if follow.end_follows() {
                format!("{}.i_in_end", CONTEXT)
            } else {
                format!("{}.i_in", CONTEXT)
            };

            // This seems like any grammar would have to obey this rule,
            // (*unless* it has unused non-terminals...).
            assert!(follow.end_follows() || follow.terms().len() > 0);

            let mut terms = first.into_terms();
            terms.union_(follow.into_terms());
            let all_terms: String = terms.iter()
                .map(|t| format!("C::Term::from_char('{}'),", t)).collect();

            // FIXME: we might special-case when the set of terms *is*
            // the universe of terminals, and just let that produce
            // the expression `true` in that scenario.
            //
            // (Though in truth, it might be nice to bubble such
            // an observation up to the higher level code generator,
            // because there is a lot of redundant follow-on code
            // being generated that we could just skip based on the
            // observation.)

            Expr(format!("{}(&[{}])", follow_pred, all_terms))
        } else {
            let first_terms: String = first.terms().iter()
                .map(|t| format!("C::Term::from_char('{}'),", t)).collect();
            Expr(format!("{}.i_in(&[{}])", CONTEXT, first_terms))
        }
    }

    // `c_u := create(l, c_u, j)`
    fn create(&self,
              l: Self::Label) -> Self::Command {
        Command::One(format!("{}.create({});", CONTEXT, l.render_use()))
    }

    // `add(l, c_u, j)
    fn add(&self, l: Self::Label) -> Self::Command {
        Command::One(format!("{}.add_s({});", CONTEXT, l.render_use()))
    }

    // `pop(c_u, j)`
    fn pop(&self) -> Self::Command {
        Command::One(format!("{}.pop();", CONTEXT))
    }
}

fn all_nonterms(rb: &RustBackend) -> Vec<NonTerm> {
    rb.0.rules.iter().map(|r|r.left).collect()
}

fn all_labels(rb: &RustBackend) -> Vec<Label> {
    let mut labels = vec![Label("_0")];
    for &Rule { left, ref right_hands } in &rb.0.rules {
        labels.push(rb.nonterm_label(left));
        for (i, alt) in right_hands.iter().enumerate() {
            labels.push(rb.alternate_label((left, i)));
            for sym in alt {
                if let &Sym::N { name, x } = sym {
                    labels.push(rb.return_label(left, (name, x)));
                }
            }
        }
    }
    labels
}

const DB_MACRO_DEFINITION: &'static str = r#"
macro_rules! db {
    ($($tt:expr),*) => {
        // println!($($tt),*)
    }
}"#;

const GOTO_MACRO_DEFINITION: &'static str = r#"
    macro_rules! goto {
        ($l:expr) => {
            { db!("goto {:?} to {:?}", pc, $l);
              pc = $l;
              continue;
            }
        }
    }
"#;

fn l0_arm() -> String {
    format!(r#"
            // This is kernel of the loop, but it is *not* where
            // we start (note that `pc` is set to `L::L_S` for some `S` above).
            L::_0 => {{
                match {context}.r_pop() {{
                    Some(Desc(l, u, j)) => {{
                        {context}.set_s(u);
                        {context}.set_i(j);
                        goto!( l );
                    }}
                    None => {{
                        if {context}.r_seen_contains(
                            &Desc(L::_0,
                                  Stack({context}.g_dummy()),
                                  InputPos({context}.i_len()))) {{
                            return Ok(C::Success::default());
                        }} else {{
                            return Err(C::ParseError::default());
                        }}
                    }}
                }}
            }}
"#,
            context=CONTEXT)
}


fn impl_for_nonterm<E:Copy>(n: NonTerm, g: &Grammar<E>) -> String {
    let first = g.first(&vec![Sym::N { name: n, x: () }]);
    let pred = if first.is_nullable() {
        format!("{}.i_in_end", CONTEXT)
    } else {
        format!("{}.i_in", CONTEXT)
    };
    let terms: String =
        first.into_terms().into_iter()
        .map(|term| format!("C::Term::from_char('{}'),", term))
        .collect();
    format!(r#"
    impl StartNonTerm<Label> for {nt} {{
        /// Returns FIRST(N$) for nonterm N (= nt).
        fn first_end<'g, C:Context<'g, Label>>(self, {context}: &C) -> bool {{
            {pred}(&[{terms}])
        }}

        fn to_label(self) -> Label {{
            Label::L_{nt}
        }}
    }}
    "#, nt=n, pred=pred, terms=terms, context=CONTEXT)
}

fn prefix(rb: &RustBackend) -> String {
    let labels: String = rb.all_labels().into_iter()
        .map(|label| format!("    {},\n", label.render_def()))
        .collect();
    let nonterm_variants: String = rb.all_nonterms().into_iter()
        .map(|nonterm| format!("    {},\n", nonterm))
        .collect();
    let nonterm_types: String = rb.all_nonterms().into_iter()
        .map(|nonterm| format!(r##"
    #[derive(Copy, Clone, PartialEq, Eq, Debug)]
    pub struct {};
"##, nonterm))
        .collect();
    let nonterm_impls: String =
        rb.all_nonterms().into_iter()
        .map(|nonterm| format!("{}\n\n", rb.impl_for_nonterm(nonterm, &rb.0)))
        .collect();
    format!(r###"
use pgy_runtime::*;

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Label {{
{labels}
}}

impl LabelZero for Label {{
    fn label_zero() -> Self {{ Label::_0 }}
}}

#[cfg(kill_me)]
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum NonTerm {{
{nonterm_variants}
}}

pub mod nonterm {{
    use pgy_runtime::{{Context, FromChar, StartNonTerm}};
    use super::{{Label}};

{nonterm_types}

{nonterm_impls}
}}

{db_macro_definition}
pub fn parse<'g, C:Context<'g, Label>, S:Copy+StartNonTerm<Label>>({context}: &mut C, s: S) -> Result<C::Success, C::ParseError> {{
    use self::Label as L;

    let mut pc;
    {goto_macro_definition}

    // pseudo `if (I[0] in FIRST(S$)) then goto L_S else report fail
    // FIXME
    if s.first_end({context}) {{
        // self::Label::from_name(&format!("L_{{}}", start_name)).unwrap();
        pc = s.to_label();
    }} else {{
        {init_parse_failure}
    }}

    loop {{
        pc = match pc {{
            {l0_arm}
"###,
            labels=labels,
            nonterm_types=nonterm_types,
            nonterm_variants=nonterm_variants,
            nonterm_impls=nonterm_impls,
            db_macro_definition = DB_MACRO_DEFINITION,
            goto_macro_definition = GOTO_MACRO_DEFINITION,
            l0_arm = l0_arm(),
            context=CONTEXT,
            init_parse_failure=rb.report_parse_failure("init").render(),
            )
}

fn suffix(_rb: &RustBackend) -> String {
    format!(r###"        }}
    }}
}}
"###,
            )
}
