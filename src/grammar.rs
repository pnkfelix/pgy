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

pub type Terms = Set<TermName>;
pub type TermsEm = Set<TermEm>;
pub type TermsEnd = Set<TermEnd>;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Sym {
    T(TermName),
    N(NontermName),
}

impl From<&'static str> for Sym {
    fn from(name: &'static str) -> Sym { Sym::N(name) }
}

impl From<char> for Sym {
    fn from(name: char) -> Sym { Sym::T(name) }
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
pub struct Rule {
    left: NontermName,
    right_hands: Vec<Vec<Sym>>,
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

struct PreGrammar0 { rules: Vec<Rule> }
struct PreGrammar1 { rules: Vec<Rule>, nullable: Nullable }
struct PreGrammar2 { rules: Vec<Rule>, nullable: Nullable, firsts: Firsts }

pub struct PreGrammar3 {
    rules: Vec<Rule>,
    nullable: Nullable,
    firsts: Firsts,
    follows: Follows,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Grammar {
    rules: Vec<Rule>,
    nullable: Nullable,
    firsts: Firsts,
    follows: Follows,
}

fn all_left_unique(rules: &[Rule]) -> bool {
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
        let follows = identify_follows(&rules[..], &nullable, &firsts);
        PreGrammar3 {
            rules: rules,
            nullable: nullable,
            firsts: firsts,
            follows: follows,
        }
    }
}

impl PreGrammar3 {
    fn finalize(self) -> Grammar {
        Grammar {
            rules: self.rules,
            nullable: self.nullable,
            firsts: self.firsts,
            follows: self.follows,
        }
    }
}

fn identify_nullables(rules: &[Rule]) -> Nullable {
    let mut changed = true;
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
                        Sym::N(n) => if !nullable.contains(n) {
                            continue 'rh;
                        }
                    }
                }

                // if we reach this point, then all elements of
                // `right_hand` (which may itself be empty) are nullable
                // non-terminals, and thus `left` is itself nullable.

                nullable.insert(left);
                changed = true;
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
    let mut changed = true;
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
                        Sym::N(n) => {
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

fn identify_follows(rules: &[Rule],
                    nullable: &Nullable,
                    firsts: &Firsts) -> Follows {
    let mut follows = HashMap::new();
    for rule in rules {
        follows.insert(rule.left, set());
    }

    let mut changed = true;
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
                        Sym::N(n) => {
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

                for p in &prevs {
                    let follows_left = follows[left].clone();
                    for t in follows_left {
                        follows.get_mut(p).unwrap().add(t, &mut changed);
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

    follows
}

impl Grammar {
    fn new(rules: Vec<Rule>) -> Grammar {
        assert!(all_left_unique(&rules[..]));
        // TODO: add checks that all non-terminals are live and defined

        let pg0 = PreGrammar0 { rules: rules };
        let pg1 = pg0.identify_nullables();
        let pg2 = pg1.identify_firsts();
        let pg3 = pg2.identify_follows();

        pg3.finalize()
    }
}

impl Grammar {
    fn start(&self) -> NontermName {
        self.rules[0].left
    }

    fn first_t(&self, a: NontermName) -> &[TermName] {
        &self.firsts[a][..]
    }

    fn follow_t(&self, a: NontermName) -> &[TermName] {
        &self.follows[a][..]
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
