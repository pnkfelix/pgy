use std::collections::{HashMap, HashSet};

pub type TermName = char;
pub type NontermName = &'static str;

/// (for now) using Vec to represent sets of things
pub type Set<T> = Vec<T>;
fn set<T>() -> Set<T> { vec![] }

trait SetUpdate<T> {
    fn add(&mut self, t: T, changed: &mut bool);
    fn add_(&mut self, t: T);
}

impl<T:Eq> SetUpdate<T> for Set<T> {
    fn add(&mut self, t: T, changed: &mut bool) {
        if !self.contains(&t) {
            self.push(t);
            *changed = true;
        }
    }
    fn add_(&mut self, t: T) {
        if !self.contains(&t) {
            self.push(t);
        }
    }
}

trait SetCompare {
    fn null_intersects(&self, other: &Self) -> bool;
    fn same_contents(&self, other: &Self) -> bool;
}

impl<T:Eq> SetCompare for Set<T> {
    fn null_intersects(&self, other: &Self) -> bool {
        for a in self {
            for b in other {
                if a == b {
                    return false;
                }
            }
        }
        return true;
    }
    fn same_contents(&self, other: &Self) -> bool {
        for a in self {
            if !other.contains(a) { return false; }
        }
        for b in other {
            if !self.contains(b) { return false; }
        }
        return true;
    }
}

impl SetCompare for TermsEm {
    fn null_intersects(&self, other: &Self) -> bool {
        self.terms.null_intersects(&other.terms) &&
            // FIXME: is the right way to handle `$`?
            !(self.is_nullable && other.is_nullable)
    }
    fn same_contents(&self, other: &Self) -> bool {
        self.terms.same_contents(&other.terms) &&
            self.is_nullable == other.is_nullable
    }
}

pub type Nonterms = Set<NontermName>;
pub type Terms = Set<TermName>;
#[derive(Debug)]
pub struct TermsEm {
    terms: Terms,
    is_nullable: bool,
}

pub struct TermsEnd {
    terms: Terms,
    // I'm following the definition of FOLLOW(A) given in the GLL
    // paper, which includes {$} if A is nullable.
    //
    // (This does not seem quite right to me though; the question of
    // whether `$` can follow A should depend on the various contexts
    // in which A appears. But it may be a technical detail that allows
    // their definition of the LL(1)-ness of a nonterminal A to go through.
    is_nullable: bool,
    end_follows: bool,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Sym<Extra:Copy=()> {
    T(TermName),

    // Each occurrence of a non-terminal in a grammar is given a `num`
    // that is unique relative to the whole grammar. (Effectively
    // this allows easy mapping of any occurrence to the context
    // where it occurred within the grammar.)
    N { name: NontermName, x: Extra },
}

impl From<&'static str> for Sym<()> {
    fn from(name: &'static str) -> Sym<()> { Sym::N { name: name, x: () } }
}

impl<E:Copy> From<char> for Sym<E> {
    fn from(name: char) -> Sym<E> { Sym::T(name) }
}

pub enum TermEm {
    T(TermName),
    MT,
}

pub enum TermEnd {
    T(TermName),
    End,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Rule<E:Copy=()> {
    left: NontermName,
    right_hands: Vec<Vec<Sym<E>>>,
}

macro_rules! rule {
    ($l:ident ::= $( ( $($e:expr)* ));*) => {
        Rule {
            left: stringify!($l),
            right_hands: vec![$(vec![$($e.into()),*]),*]
        }
    }
}

pub type Nullable = HashSet<NontermName>;
pub type Firsts = HashMap<NontermName, Terms>;
pub type Follows = HashMap<NontermName, Terms>;

struct PreGrammar0<E:Copy=()> { rules: Vec<Rule<E>> }
struct PreGrammar1<E:Copy=()> { rules: Vec<Rule<E>>, nullable: Nullable }
struct PreGrammar2<E:Copy=()> { rules: Vec<Rule<E>>, nullable: Nullable, firsts: Firsts }

pub struct PreGrammar3<E:Copy=()> {
    rules: Vec<Rule<E>>,
    nullable: Nullable,
    firsts: Firsts,
    follows: Follows,
    end_follows: Nonterms
}

pub struct PreGrammar4<E:Copy=()> {
    rules: Vec<Rule<E>>,
    nullable: Nullable,
    firsts: Firsts,
    follows: Follows,
    end_follows: Nonterms,
    ll1s: Nonterms,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Grammar<E:Copy=()> {
    rules: Vec<Rule<E>>,
    nullable: Nullable,
    firsts: Firsts,
    follows: Follows,
    end_follows: Nonterms,
    ll1s: Nonterms,
}

fn all_left_unique<E:Copy>(rules: &[Rule<E>]) -> bool {
    let mut lefts: Vec<_> = rules.iter().map(|r|r.left).collect();
    let old_len = lefts.len();
    lefts.sort();
    lefts.dedup();
    lefts.len() == old_len
}

impl PreGrammar0 {
    fn identify_nullables(self) -> PreGrammar1 {
        let PreGrammar0 { rules } = self;
        let nullable = identify_nullables(&rules[..]);
        PreGrammar1 {
            rules: rules,
            nullable: nullable
        }
    }
}

impl PreGrammar1 {
    fn identify_firsts(self) -> PreGrammar2 {
        let PreGrammar1 { rules, nullable } = self;
        let firsts = identify_firsts(&rules[..], &nullable);
        PreGrammar2 {
            rules: rules,
            nullable: nullable,
            firsts: firsts,
        }
    }
}

impl PreGrammar2 {
    fn identify_follows(self) -> PreGrammar3 {
        let PreGrammar2 { rules, nullable, firsts } = self;
        let (follows, end_follows) = identify_follows(&rules[..], &nullable, &firsts);
        PreGrammar3 {
            rules: rules,
            nullable: nullable,
            firsts: firsts,
            follows: follows,
            end_follows: end_follows,
        }
    }
}

#[derive(Copy, Clone)]
struct FirstContext<'a> {
    nullable: &'a Nullable,
    firsts: &'a Firsts,
}

#[derive(Copy, Clone)]
struct FollowContext<'a> {
    follows: &'a Follows,
    end_follows: &'a Nonterms,
}

impl PreGrammar3 {
    fn identify_ll1s(self) -> PreGrammar4 {
        let PreGrammar3 {
            rules: rules,
            nullable: nullable,
            firsts: firsts,
            follows: follows,
            end_follows: end_follows,
        } = self;

        let ll1s = {
            let fic = FirstContext { nullable: &nullable,
                                     firsts: &firsts };
            let foc = FollowContext { follows: &follows,
                                      end_follows: &end_follows };

            identify_ll1s(&rules[..], fic, foc)
        };

        PreGrammar4 {
            rules: rules,
            nullable: nullable,
            firsts: firsts,
            follows: follows,
            end_follows: end_follows,
            ll1s: ll1s,
        }
    }
}

fn first(ctxt: FirstContext, alpha: &[Sym]) -> TermsEm {
    let FirstContext { nullable, firsts } = ctxt;
    let mut first = Terms::new();
    let mut broke_before_end = false;
    for s in alpha {
        match *s {
            Sym::T(term) => {
                first.add_(term);
                broke_before_end = true;
                break;
            }
            Sym::N { name: nonterm, .. } => {
                for t in &firsts[nonterm] {
                    first.add_(*t);
                }
                if nullable.contains(nonterm) {
                    continue;
                } else {
                    broke_before_end = true;
                    break;
                }
            }
        }
    }

    TermsEm {
        terms: first,
        is_nullable: !broke_before_end,
    }
}

// A Nonterm A is LL(1) if
// (i) A ::= \alpha, A ::= \beta imply FIRST(\alpha) & FIRST(\beta) = null, and
// (ii) if A =>^* epsilon then FIRST(A) & FOLLOW(A) = null.
fn identify_ll1s(rules: &[Rule],
                 fic: FirstContext,
                 foc: FollowContext) -> Nonterms {
    let mut nonterms: Nonterms = set();

    // this is already asserted by `Grammar::new`, so a debug_assert
    // is really just a reminder here.
    debug_assert!(all_left_unique(rules));

    'next_rule: for rule in rules {
        let &Rule { left: A, ref right_hands } = rule;
        for (i, ref alpha) in right_hands.iter().enumerate() {
            for ref beta in &right_hands[(i+1)..] {
                let first_alpha = first(fic, alpha);
                let first_beta = first(fic, beta);
                if !first_alpha.null_intersects(&first_beta) {
                    println!("rule: {:?} is not LL(1) because \
                             {:?} = FIRST({:?}) intersects FIRST({:?}) = {:?}",
                             rule, first_alpha, alpha, beta, first_beta);
                    continue 'next_rule;
                }
            }
        }

        if fic.nullable.contains(A) {
            let a_first = &fic.firsts[A];
            let a_follow = &foc.follows[A];
            if !a_first.null_intersects(&a_follow) {
                println!("rule: {:?} is not LL(1) because it is nullable and \
                          {:?} = FIRST({:?} intersects FOLLOW({:?}) = {:?}",
                         rule, a_first, A, A, a_follow);
                continue 'next_rule;
            }

            // FIXME: Is this the right way to deal with `$` in the
            // abstract sets?
            if !foc.end_follows.contains(&A) {
                println!("rule: {:?} is not LL(1) because \
                          `$` not in FOLLOW({:?})",
                         rule, A);
                continue 'next_rule;
            }
        }

        nonterms.add_(A);
    }

    nonterms
}

impl PreGrammar4 {
    fn finalize(self) -> Grammar {
        Grammar {
            rules: self.rules,
            nullable: self.nullable,
            firsts: self.firsts,
            follows: self.follows,
            end_follows: self.end_follows,
            ll1s: self.ll1s,
        }
    }
}

fn identify_nullables(rules: &[Rule]) -> Nullable {
    let mut changed = true;;
    let mut nullable = HashSet::new();
    while changed {
        changed = false;

        'rules: for rule in rules {
            let Rule { left, ref right_hands } = *rule;
            if nullable.contains(left) {
                // we have already id'ed this nonterm as nullable
                continue;
            }
            'rh: for right_hand in right_hands {
                for s in right_hand {
                    match *s {
                        Sym::T(_) => continue 'rh,
                        Sym::N { name: n, .. } => if !nullable.contains(n) {
                            continue 'rh;
                        }
                    }
                }

                // if we reach this point, then all elements of
                // `right_hand` (which may itself be empty) are nullable
                // non-terminals, and thus `left` is itself nullable.

                nullable.insert(left);
                changed = true;;
                continue 'rules;
            }
        }
    }

    nullable
}

fn identify_firsts(rules: &[Rule], nullable: &Nullable) -> Firsts {
    let mut firsts: Firsts = HashMap::new();
    for rule in rules {
        firsts.insert(rule.left, set());
    }
    let mut changed = true;;
    while changed {
        changed = false;

        'rules: for rule in rules {
            let Rule { left, ref right_hands } = *rule;
            let mut old_first = firsts[left].clone();

            'rh: for right_hand in right_hands {
                for s in right_hand {
                    match *s {
                        Sym::T(t) => {
                            old_first.add(t, &mut changed);
                            // done with this right-hand side.
                            continue 'rh;
                        }
                        Sym::N { name: n, .. } => {
                            let n_first = firsts.get(n).unwrap_or_else(|| {
                                panic!("nonterm {} with no rule", left);
                            });
                            for s in n_first {
                                old_first.add(*s, &mut changed);
                            }

                            if !nullable.contains(n) {
                                // done with this right-hand side.
                                continue 'rh;
                            }
                        }
                    }
                }
            }

            if changed {
                firsts.insert(left, old_first);
            }
        }
    }

    // sort all the sets to ease comparing them in tests (and i
    // supposed I could also use this to allow binary-search based
    // lookup post-construction).
    for (_, first) in firsts.iter_mut() {
        first.sort();
    }

    firsts
}

// Returns `(follows, end_follows)` -- where `follows` is a map from
// Nonterm to the set of terms that can follow it, and `end_follows`
// is the set of Nonterms that can be followed by end of the input
// (aka `$`).
fn identify_follows(rules: &[Rule],
                    nullable: &Nullable,
                    firsts: &Firsts) -> (Follows, Nonterms) {
    let mut follows = HashMap::new();
    let mut end_follows = Vec::new();

    for rule in rules {
        follows.insert(rule.left, set());
    }
    end_follows.add_(rules[0].left);

    let mut changed = true;;
    while changed {
        changed = false;
        for rule in rules {
            let Rule { left, ref right_hands } = *rule;

            let mut prevs: Vec<NontermName> = Vec::new();
            for right_hand in right_hands {
                prevs.clear();
                for s in right_hand {
                    match *s {
                        Sym::T(t) => {
                            for p in &prevs {
                                follows.get_mut(p).unwrap().add(t, &mut changed);
                            }
                            prevs.clear();
                        }
                        Sym::N { name:n, .. } => {
                            for t in &firsts[n] {
                                for p in &prevs {
                                    follows.get_mut(p).unwrap().add(*t, &mut changed);
                                }
                            }
                            if !nullable.contains(n) {
                                prevs.clear();
                            }
                            prevs.push(n);
                        }
                    }
                }

                // at the end of the right-hand side, we now propagate
                // any t that had been previously found in the follow-set
                // for the left-hand symbol.
                // e.g. for a rule like:
                //
                //   S ::= B C | A S 'd'
                //   C ::= 'c' | epsilon
                //
                // 'd' is in FOLLOW(S); therefore we must conclude
                // that 'd' is in FOLLOW(B), because that right-hand
                // side `B C` may occur in a context `A [] 'd'` and
                // the C is nullable (from the epsilon),

                let follows_left = follows[left].clone();
                for p in &prevs {
                    for t in &follows_left {
                        follows.get_mut(p).unwrap().add(*t, &mut changed);
                    }
                }
                if end_follows.contains(&left) {
                    for p in &prevs {
                        end_follows.add(p, &mut changed);
                    }
                }
            }
        }
    }

    // sort all the sets to ease comparing them in tests (and i
    // supposed I could also use this to allow binary-search based
    // lookup post-construction).
    for (_, follow) in follows.iter_mut() {
        follow.sort();
    }

    (follows, end_follows)
}

impl Grammar {
    pub fn new(rules: Vec<Rule>) -> Grammar {
        assert!(all_left_unique(&rules[..]));
        // TODO: add checks that all non-terminals are live and defined

        let pg0 = PreGrammar0 { rules: rules };
        let pg1 = pg0.identify_nullables();
        let pg2 = pg1.identify_firsts();
        let pg3 = pg2.identify_follows();
        let pg4 = pg3.identify_ll1s();

        pg4.finalize()
    }
}

impl Rule<()> {
    pub fn tag_nonterminals(self, ctrs: &mut HashMap<NontermName, usize>) -> Rule<usize> {
        let Rule { left, right_hands } = self;
        Rule {
            left: left,
            right_hands: right_hands.into_iter().map(|alt| {
                alt.into_iter().map(|sym| match sym {
                    Sym::T(t) => Sym::T(t),
                    Sym::N { name: name, x: () } => {
                        let ctr = ctrs.entry(name).or_insert(0);
                        let n = Sym::N { name: name, x: *ctr };
                        *ctr = *ctr + 1;
                        n
                    }
                }).collect()
            }).collect()
        }
    }
}

impl Grammar {
    pub fn tag_nonterminals(self) -> Grammar<usize> {
        let Grammar {
            rules, nullable, firsts, follows, end_follows, ll1s
        } = self;
        let mut c = HashMap::with_capacity(firsts.len());
        let new_rules = rules.into_iter().map(|r|r.tag_nonterminals(&mut c)).collect();
        Grammar {
            rules: new_rules,
            nullable: nullable,
            firsts: firsts,
            follows: follows,
            end_follows: end_follows,
            ll1s: ll1s,
        }
    }
}

impl Grammar {
    pub fn start(&self) -> NontermName {
        self.rules[0].left
    }

    fn first_t(&self, a: NontermName) -> &[TermName] {
        &self.firsts[a][..]
    }

    fn follow_t(&self, a: NontermName) -> &[TermName] {
        &self.follows[a][..]
    }
}

impl Grammar {
    pub fn first(&self, alpha: &[Sym]) -> TermsEm {
        let fic = FirstContext {
            nullable: &self.nullable,
            firsts: &self.firsts,
        };
        first(fic, alpha)
    }
}

impl Grammar {
    pub fn follow(&self, a: NontermName) -> TermsEnd {
        let terms: Terms = self.follow_t(a).iter().cloned().collect();
        TermsEnd {
            terms: terms,
            is_nullable: self.nullable.contains(a),
            end_follows: self.end_follows.contains(&a),
        }
    }
}

#[cfg(test)]
fn demo_grammar() -> Grammar {
    Grammar::new(vec![rule!(S ::= ("A" "S" 'd'); ("B" "S"); ()),
                      rule!(A ::= ('a'); ('c')),
                      rule!(B ::= ('a'); ('b'))])
}

#[test]
fn test_nullable() {
    let g = demo_grammar();
    assert_eq!(g.nullable.iter().collect::<Vec<_>>(), [&"S"]);
}

#[test]
fn test_first_t() {
    let g = demo_grammar();
    assert_eq!(g.first_t("S"), ['a', 'b', 'c']);
    assert_eq!(g.first_t("A"), ['a', 'c']);
    assert_eq!(g.first_t("B"), ['a', 'b']);
}

#[test]
fn test_follow_t() {
    let g = demo_grammar();
    assert_eq!(g.follow_t("S"), ['d']);
    assert_eq!(g.follow_t("A"), ['a', 'b', 'c', 'd']);

    // S -> BS -> BBS -> BaS
    // S -> BS -> BBS -> BbS
    // S -> BS -> BASd -> BcSd
    // S -> ASd -> ABSd -> ABd
    assert_eq!(g.follow_t("B"), ['a', 'b', 'c', 'd']);
}

#[test]
fn test_ll1_nonterms() {
    let g = demo_grammar();
    assert!(g.ll1s.same_contents(&vec!["A", "B"]));
}

#[test]
fn test_ll1_tagging() {
    let g = demo_grammar();
    let g = g.tag_nonterminals();
    assert_eq!(g.rules, vec![Rule {
        left: "S",
        right_hands: vec![vec![Sym::N { name: "A", x: 0 },
                               Sym::N { name: "S", x: 0 },
                               Sym::T('d')],
                          vec![Sym::N { name: "B", x: 0 },
                               Sym::N { name: "S", x: 1 }],
                          vec![]], }, Rule {
        left: "A",
        right_hands: vec![vec![Sym::T('a')],
                          vec![Sym::T('c')]], }, Rule {
        left: "B",
        right_hands: vec![vec![Sym::T('a')],
                          vec![Sym::T('b')]], }
                             ]);
}
