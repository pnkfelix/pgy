use grammar::{Grammar, NontermName, Rule, Sym, TermName};

pub fn codegen<B:BackendText>(back: &mut B) -> String where
    // IMO these should not be necessary, see Rust issue #29143
    B::Block: RenderIndent
{
    let mut s = String::new();
    s = s + &back.prefix();
    let indent = back.rule_indent_preference();
    let mut cg = Codegen::new(back);
    for rule in &cg.grammar().rules {
        // FIXME: make `fn on_rule` take a `&Rule` instead of cloning.
        let (c, blocks) = cg.on_rule(rule.clone());
        let l_a = cg.backend.nonterm_label(rule.left);
        let b = cg.backend.block(l_a, c);
        s = s + &b.render_indent(indent);
        let blocks: String = blocks.iter()
            .map(|b|b.render_indent(indent))
            .collect();
        s = s + &blocks;
    }
    s = s + &cg.backend.suffix();

    return s;
}

pub trait RenderIndent {
    fn render_indent(&self, usize) -> String;
    fn render(&self) -> String { self.render_indent(0) }
}

pub trait BackendText: Backend where Self::Block: RenderIndent {
    fn prefix(&self) -> String;
    fn suffix(&self) -> String;
    fn rule_indent_preference(&self) -> usize;
}

pub trait Backend {
    type Command;
    type Expr;
    type Label: Clone;
    type Block;

    fn grammar(&self) -> &Grammar<usize>;

    // (The label generators are all non `&mut self` because in
    // principle we should generate the labels ahead of time
    // for any given grammar.)

    /// L_0 is the central loop of the parser.
    fn label_0(&self) -> Self::Label;

    /// R_A_k labels function call return to nonterm N from the
    /// call associated with A_k. (A_k is unique in the grammar
    /// and thus we can derive `N` from it in the formalism, but
    /// it seems simpler to just pass it along in this API here.)
    fn return_label(&self, n: NontermName, a_k: (NontermName, usize)) -> Self::Label;

    /// L_A labels parse function for A.
    fn nonterm_label(&self, a: NontermName) -> Self::Label;

    /// L_A_i labels function for parsing ith alternate α_i of A.
    fn alternate_label(&self,
                       a_i: (NontermName, usize)) -> Self::Label;


    /// `L: C`
    /// (note that `C` must have control flow ending in goto...)
    fn block(&self, l: Self::Label, c: Self::Command) -> Self::Block;

    /// Execute this command to report the parse attempt failed.
    fn report_parse_failure(&self, &str) -> Self::Command;

    /// Execute this command if something unexpected happened
    /// in the generated code.
    fn panic_fail(&self, &str) -> Self::Command;

    /// the no-op command makes some constructions easier.
    fn no_op(&self) -> Self::Command;

    /// `cmd1, cmd2`
    fn seq(&self,
           cmd1: Self::Command,
           cmd2: Self::Command) -> Self::Command;

    /// `if test { then }
    fn if_(&self,
           test: Self::Expr,
           then: Self::Command) -> Self::Command;

    /// `if test { then } else { else_ }`
    fn if_else(&self,
               test: Self::Expr,
               then: Self::Command,
               else_: Self::Command) -> Self::Command;

    /// `j := j + 1`
    fn increment_curr(&self) -> Self::Command;

    /// let L = label;
    /// `goto L`
    fn goto(&self, label: Self::Label) -> Self::Command;

    /// this comes up a lot.
    fn goto_l0(&self) -> Self::Command {
        let l0 = self.label_0();
        self.goto(l0)
    }

    /// `I[j] == a`
    fn curr_matches_term(&self, a: TermName) -> Self::Expr;

    /// let x = I[j]; let N = n;
    /// `x in FIRST(N$)`
    ///
    /// The leading optional component in alpha is meant to be
    /// the first element of alpha, if it is present at all.
    fn test_end<E:Copy>(&self, n: NontermName) -> Self::Expr;

    /// let x = I[j]; let α = alpha;
    /// `x in FIRST(α) or empty in FIRST(α) and x in FOLLOW(A)`
    ///
    /// The leading optional component in alpha is meant to be
    /// the first element of alpha, if it is present at all.
    fn test<E:Copy>(&self,
                    a: NontermName,
                    alpha: (Option<NontermName>, &[Sym<E>])) -> Self::Expr;

    /// `c_u := create(l, c_u, j)`
    fn create(&self,
              l: Self::Label) -> Self::Command;

    /// `add(l, c_u, j)
    fn add(&self, l: Self::Label) -> Self::Command;

    /// `pop(c_u, j)`
    fn pop(&self) -> Self::Command;
}

pub struct Codegen<'a, B:Backend+'a> {
    pub backend: &'a mut B,
}

impl<'a, C:Backend> Codegen<'a, C> {
    pub fn new(back: &'a mut C) -> Self {
        Codegen { backend: back }
    }
    pub fn grammar(&self) -> &Grammar<usize> { self.backend.grammar() }

    /// code(aα, j, X) = if I[j] = a {j := j+1} else {goto L_0}
    pub fn on_term(&self, a: TermName) -> C::Command {
        let b = &self.backend;
        let matches = b.curr_matches_term(a);
        let next_j = b.increment_curr();
        let goto_l0 = b.goto_l0();
        b.if_else(matches, next_j, goto_l0)
    }

    /// code(A_kα, j, X) =
    ///   if test(I[j], X, A_k α) {
    ///      c_u := create(R_A_k, c_u, j), goto L_A
    ///   } else {
    ///      goto L_0
    ///   }
    ///   R_A_k:
    pub fn on_nonterm_instance<E:Copy>(&self,
                                   (a, k): (NontermName, usize),
                                   alpha: &[Sym<E>],
                                   x: NontermName) -> (C::Command, C::Label) {
        let b = &self.backend;
        let matches = b.test(x, (Some(a), alpha));
        let r_a_k = b.return_label(x, (a, k));
        let create = b.create(r_a_k);
        let l_a = b.nonterm_label(a);
        let goto_la = b.goto(l_a);
        let create_then_goto_la = b.seq(create, goto_la);
        let goto_l0 = b.goto_l0();
        let c = b.if_else(matches, create_then_goto_la, goto_l0);
        let l = b.return_label(x, (a, k));
        (c, l)
    }

    /// code(α, j, X) = ...
    ///
    /// (driver for calling either of on_term/on_nonterm_instance)
    pub fn on_symbols(&self,
                      alpha: &[Sym<usize>],
                      x: NontermName) -> (C::Command, Option<C::Label>) {
        assert!(alpha.len() > 0);
        let (s_0, alpha) = alpha.split_at(1);
        match s_0[0] {
            Sym::T(t) =>
                (self.on_term(t), None),
            Sym::N { name: a, x: x_ } => {
                let (c, l) = self.on_nonterm_instance((a, x_), alpha, x);
                (c, Some(l))
            }
        }
    }

    /// Given alpha = x1 x2 .. x_f, shorthand for
    ///
    ///   code(x1    .. x_f, j, A)
    ///   code(   x2 .. x_f, j, A)
    ///   ...
    ///   code(         x_f, j, A)
    ///
    /// Each `code` maps to a command and (potentially) a trailing label;
    /// therefore concatenating the codes results in a leading command
    /// and a sequence of blocks.
    /// The above maps to a command and a sequence of bl
    pub fn on_symbols_in_prod(&self,
                              alpha: &[Sym<usize>],
                              a: NontermName,
                              end_with: C::Command)
                              -> (C::Command, Vec<C::Block>) {
        let mut c = self.backend.no_op();

        enum BuildState<C:Backend> {
            FirstCommand,
            MakeEndBlock {
                first: C::Command,
                then: Vec<C::Block>,
                end: C::Label
            }
        }

        let mut bs: BuildState<C> = BuildState::FirstCommand;
        for i in 0..alpha.len() {
            let (c2, opt_label) = self.on_symbols(&alpha[i..], a);
            c = self.backend.seq(c, c2);
            if let Some(l) = opt_label {
                bs = match bs {
                    BuildState::FirstCommand =>
                        BuildState::MakeEndBlock {
                            first: c,
                            then: Vec::new(),
                            end: l
                        },
                    BuildState::MakeEndBlock {first,mut then,end} => {
                        let b = self.backend.block(end, c);
                        then.push(b);
                        BuildState::MakeEndBlock {
                            first: first,
                            then: then,
                            end: l
                        }
                    }
                };
                c = self.backend.no_op();
            }
        }

        match bs {
            BuildState::FirstCommand => {
                c = self.backend.seq(c, end_with);
                return (c, Vec::new());
            }
            BuildState::MakeEndBlock { first, mut then, end } => {
                c = self.backend.seq(c, end_with);
                let b = self.backend.block(end, c);
                then.push(b);
                return (first, then);
            }
        }
    }

    /// code(A ::= empty, j) = pop(c_u, j); goto L_0
    ///
    /// code(A ::= <term> x_2 .. x_f , j) =
    ///   j := j + 1
    ///   code(x2    .. x_f, j, A)
    ///   code(   x3 .. x_f, j, A)
    ///   ...
    ///   code(         x_f, j, A)
    ///   pop(c_u, j),
    ///   goto L_0
    ///
    /// code(A ::= X_l x_2 .. x_f, j) =
    ///   c_u := create(R_X_l, c_u, j);
    ///   goto L_X;
    ///   R_X_l: code(x_2     .. x_f, j, A)
    ///          code(    x_3 .. x_f, j, A)
    ///          ...
    ///          code(           x_f, j, A)
    ///          pop(c_u, j)
    ///          goto L_0

    pub fn on_production(&self,
                         a: NontermName,
                         alpha: &[Sym<usize>]) -> (C::Command,
                                                   Vec<C::Block>) {
        let end_with = {
            let b = &self.backend;
            let pop = b.pop();
            let goto_l0 = b.goto_l0();
            b.seq(pop, goto_l0)
        };

        if alpha.len() == 0 {
            return (end_with, Vec::new());
        }

        match alpha[0] {
            Sym::T(_) => {
                // The code produced here is only meant to be run if
                // we've already matched the first terminal of a
                // non-empty α.  It probably would be a good idea to
                // actually assert such a match, but whatever.

                let next_j = self.backend.increment_curr();
                let (c, blocks) =
                    self.on_symbols_in_prod(&alpha[1..], a, end_with);
                (self.backend.seq(next_j, c), blocks)
            }

            Sym::N { name: X, x: l } => {
                let r_X_l = self.backend.return_label(a, (X, l));
                let c1 = {
                    let b = &self.backend;
                    let l_X = b.nonterm_label(X);
                    let create = b.create(r_X_l.clone());
                    let goto_lX = b.goto(l_X);
                    b.seq(create, goto_lX)
                };

                let (c2, more_blocks) =
                    self.on_symbols_in_prod(&alpha[1..], a, end_with);
                let block = self.backend.block(r_X_l, c2);
                let mut blocks = Vec::with_capacity(1 + more_blocks.len());
                blocks.push(block);
                for b in more_blocks { blocks.push(b); }
                (c1, blocks)
            }
        }
    }

    /// let the rule for A be `A ::= α_1 | ... | α_t`
    ///
    /// code(A, j) if A is LL(1) nonterm =
    ///   if test(I[j], A, α_1) { goto L_A_1 }
    ///   ...
    ///   else if test(I[j], A, α_t) { goto L_A_t }
    ///   // (assert unreachable here?)
    /// L_A_1: code(A ::= α_1, j)
    /// ...
    /// L_A_t: code(A ::= α_t, j)
    ///
    /// code(A, j) if A is not LL(1) nonterm =
    ///   if test(I[j], A, α_1) { add(L_A_1, c_u, j) }
    ///   ...
    ///   if test(I[j], A, α_1) { add(L_A_t, c_u, j) }
    ///   goto L_0
    /// L_A_1: code(A ::= α_1, j)
    /// ...
    /// L_A_t: code(A ::= α_t, j)
    ///
    pub fn on_rule(&self,
                   r: Rule<usize>) -> (C::Command,
                                       Vec<C::Block>) {
        let Rule { left: a, right_hands: ref alphas } = r;
        let c = if self.grammar().ll1s.contains(&a) {
            let b = &self.backend;
            let mut c = b.no_op();
            for (i, alpha) in alphas.iter().enumerate() {
                let test = b.test(a, (None, alpha));
                let l_a_i = b.alternate_label((a, i));
                let goto_l_a_i = b.goto(l_a_i);
                let c2 = b.if_(test, goto_l_a_i);
                c = b.seq(c, c2);
            }
            let u = b.panic_fail(&format!("unreachable for {}", a));
            c = b.seq(c, u);
            c
        } else {
            let b = &self.backend;
            let mut c = b.no_op();
            for (i, alpha) in alphas.iter().enumerate() {
                let test = b.test(a, (None, alpha));
                let l_a_i = b.alternate_label((a, i));
                let add_l_a_i = b.add(l_a_i);
                let c2 = b.if_(test, add_l_a_i);
                c = b.seq(c, c2);
            }
            let goto_l0 = b.goto_l0();
            c = b.seq(c, goto_l0);
            c
        };

        // each call to `on_production` gives back a command and
        // a potential block; we turn each command into its
        // own block, so the total blocks is 2 * |alphas|.
        let mut blocks = Vec::with_capacity(2*alphas.len());
        for (i, alpha) in alphas.iter().enumerate() {
            let (c, more_blocks) = self.on_production(a, alpha);
            let b = &self.backend;
            let l_a_i = b.alternate_label((a, i));
            let block = b.block(l_a_i, c);
            blocks.push(block);
            for b in more_blocks { blocks.push(b); }
        }

        (c, blocks)
    }
}
