use grammar::{Grammar, NontermName, Rule, Sym, TermName};

pub trait Backend<'a> {
    type Command;
    type Expr;
    type Label: Clone;
    type Block;

    fn new(g: &'a Grammar) -> Self;

    // (The label generators are all non `&mut self` because in
    // principle we should generate the labels ahead of time
    // for any given grammar.)

    // L_0 is the central loop of the parser.
    fn label_0(&self) -> Self::Label;

    // R_A_k labels function call return lines.
    fn return_label(&self, a_k: (NontermName, usize)) -> Self::Label;

    // L_A labels parse function for A.
    fn nonterm_label(&self, a: NontermName) -> Self::Label;

    // L_A_i labels function for parsing ith alternate α_i of A.
    fn alternate_label(&self,
                       a_i: (NontermName, usize)) -> Self::Label;


    // `L: C`
    // (note that `C` must have control flow ending in goto...)
    fn block(&mut self, l: Self::Label, c: Self::Command) -> Self::Block;

    // Execute this command to report the parse attempt failed.
    fn report_parse_failure(&mut self, &str) -> Self::Command;

    // Execute this command if something unexpected happened
    // in the generated code.
    fn panic_fail(&mut self, &str) -> Self::Command;

    // the no-op command makes some constructions easier.
    fn no_op(&mut self) -> Self::Command;

    // `cmd1, cmd2`
    fn seq(&mut self,
           cmd1: Self::Command,
           cmd2: Self::Command) -> Self::Command;

    // `if test { then }
    fn if_(&mut self,
           test: Self::Expr,
           then: Self::Command) -> Self::Command;

    // `if test { then } else { else_ }`
    fn if_else(&mut self,
           test: Self::Expr,
           then: Self::Command,
           else_: Self::Command) -> Self::Command;

    // `j := j + 1`
    fn increment_curr(&mut self) -> Self::Command;

    // let L = label;
    // `goto L`
    fn goto(&mut self, label: Self::Label) -> Self::Command;

    // this comes up a lot.
    fn goto_l0(&mut self) -> Self::Command {
        let l0 = self.label_0();
        self.goto(l0)
    }

    // `I[j] == a`
    fn curr_matches_term(&mut self, a: TermName) -> Self::Expr;

    // let x = I[j]; let α = alpha;
    // `x in FIRST(α) or empty in FIRST(α) and x in FOLLOW(A)`
    //
    // The leading optional component in alpha is meant to be
    // the first element of alpha, if it is present at all.
    fn test<E:Copy>(&mut self,
                    a: NontermName,
                    alpha: (Option<NontermName>, &[Sym<E>])) -> Self::Expr;

    // `c_u := create(l, c_u, j)`
    fn create(&mut self,
              l: Self::Label) -> Self::Command;

    // `add(l, c_u, j)
    fn add(&mut self, l: Self::Label) -> Self::Command;

    // `pop(c_u, j)`
    fn pop(&mut self) -> Self::Command;
}

struct Codegen<'a, B:Backend<'a>+'a> {
    backend: &'a mut B,
    grammar: &'a Grammar,
}

impl<'a, C:Backend<'a>> Codegen<'a, C> {
    // code(aα, j, X) = if I[j] = a {j := j+1} else {goto L_0}
    fn on_term(&mut self, a: TermName) -> C::Command {
        let b = &mut self.backend;
        let matches = b.curr_matches_term(a);
        let next_j = b.increment_curr();
        let goto_l0 = b.goto_l0();
        b.if_else(matches, next_j, goto_l0)
    }

    // code(A_kα, j, X) =
    //   if test(I[j], X, A_k α) {
    //      c_u := create(R_A_k, c_u, j), goto L_A
    //   } else {
    //      goto L_0
    //   }
    fn on_nonterm_instance<E:Copy>(&mut self,
                                   (a, k): (NontermName, usize),
                                   alpha: &[Sym<E>],
                                   x: NontermName) -> C::Command {
        let b = &mut self.backend;
        let matches = b.test(x, (Some(a), alpha));
        let r_a_k = b.return_label((a, k));
        let create = b.create(r_a_k);
        let l_a = b.nonterm_label(a);
        let goto_la = b.goto(l_a);
        let create_then_goto_la = b.seq(create, goto_la);
        let goto_l0 = b.goto_l0();
        b.if_else(matches, create_then_goto_la, goto_l0)
    }

    // code(α, j, X) = ...
    //
    // (driver for calling either of on_term/on_nonterm_instance)
    fn on_symbols(&mut self,
                          alpha: &[Sym<usize>],
                          x: NontermName) -> C::Command {
        assert!(alpha.len() > 0);
        let (s_0, alpha) = alpha.split_at(1);
        match s_0[0] {
            Sym::T(t) =>
                self.on_term(t),
            Sym::N { name: a, x: x_ } =>
                self.on_nonterm_instance((a, x_), alpha, x)
        }
    }

    // Given alpha = x1 x2 .. x_f, shorthand for
    //
    //   code(x1    .. x_f, j, A)
    //   code(   x2 .. x_f, j, A)
    //   ...
    //   code(         x_f, j, A)
    fn on_symbols_in_prod(&mut self,
                          alpha: &[Sym<usize>],
                          a: NontermName) -> C::Command {
        let mut c = self.backend.no_op();
        for i in 0..alpha.len()-1 {
            let c2 = self.on_symbols(&alpha[i..], a);
            c = self.backend.seq(c, c2);
        }
        c
    }

    // code(A ::= empty, j) = pop(c_u, j); goto L_0
    //
    // code(A ::= <term> x_2 .. x_f , j) =
    //   j := j + 1
    //   code(x2    .. x_f, j, A)
    //   code(   x3 .. x_f, j, A)
    //   ...
    //   code(         x_f, j, A)
    //   pop(c_u, j),
    //   goto L_0
    //
    // code(A ::= X_l x_2 .. x_f, j) =
    //   c_u := create(R_X_l, c_u, j);
    //   goto L_X;
    //   R_X_l: code(x_2     .. x_f, j, A)
    //          code(    x_3 .. x_f, j, A)
    //          ...
    //          code(           x_f, j, A)
    //          pop(c_u, j)
    //          goto L_0

    fn on_production(&mut self,
                     a: NontermName,
                     alpha: &[Sym<usize>]) -> (C::Command,
                                               Option<C::Block>) {
        if alpha.len() == 0 {
            return (self.backend.pop(), None);
        }
        match alpha[0] {
            Sym::T(_) => {
                // The code produced here is only meant to be run if
                // we've already matched the first terminal of a
                // non-empty α.  It probably would be a good idea to
                // actually assert such a match, but whatever.

                let next_j = self.backend.increment_curr();
                let mut c = self.on_symbols_in_prod(&alpha[1..], a);
                let b = &mut self.backend;
                let mut c = b.seq(next_j, c);
                let pop = b.pop();
                c = b.seq(c, pop);
                let goto_l0 = b.goto_l0();
                (b.seq(c, goto_l0), None)
            }

            Sym::N { name: X, x: l } => {
                let r_X_l = self.backend.return_label((X, l));
                let c1 = {
                    let b = &mut self.backend;
                    let l_X = b.nonterm_label(X);
                    let create = b.create(r_X_l.clone());
                    let goto_lX = b.goto(l_X);
                    b.seq(create, goto_lX)
                };

                let mut c2 = self.on_symbols_in_prod(&alpha[1..], a);
                let b = &mut self.backend;
                let pop = b.pop();
                c2 = b.seq(c2, pop);
                let goto_l0 = b.goto_l0();
                c2 = b.seq(c2, goto_l0);
                let block = b.block(r_X_l, c2);
                (c1, Some(block))
            }
        }
    }

    // let the rule for A be `A ::= α_1 | ... | α_t`
    //
    // code(A, j) if A is LL(1) nonterm =
    //   if test(I[j], A, α_1) { goto L_A_1 }
    //   ...
    //   else if test(I[j], A, α_t) { goto L_A_t }
    //   // (assert unreachable here?)
    // L_A_1: code(A ::= α_1, j)
    // ...
    // L_A_t: code(A ::= α_t, j)
    //
    // code(A, j) if A is not LL(1) nonterm =
    //   if test(I[j], A, α_1) { add(L_A_1, c_u, j) }
    //   ...
    //   if test(I[j], A, α_1) { add(L_A_t, c_u, j) }
    //   goto L_0
    // L_A_1: code(A ::= α_1, j)
    // ...
    // L_A_t: code(A ::= α_t, j)
    //
    fn on_rule(&mut self,
               r: Rule<usize>) -> (C::Command,
                                   Vec<C::Block>) {
        let Rule { left: a, right_hands: ref alphas } = r;
        let c = if self.grammar.ll1s.contains(&a) {
            let b = &mut self.backend;
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
            let b = &mut self.backend;
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
            let (c, opt_b) = self.on_production(a, alpha);
            let b = &mut self.backend;
            let l_a_i = b.alternate_label((a, i));
            let block = b.block(l_a_i, c);
            blocks.push(block);
            if let Some(b) = opt_b { blocks.push(b); }
        }

        (c, blocks)
    }
}
