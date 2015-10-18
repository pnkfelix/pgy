

use grammar::{tiny_grammar, demo_grammar};
use codegen::rust::RustBackend;
use gll::codegen::{Backend, Codegen};

#[test]
fn demo_1() {
    for (name, g) in vec![("a", tiny_grammar::a()),
                          ("ab", tiny_grammar::ab()),
                          ("a|b", tiny_grammar::a_or_b()),
                          ("a*", tiny_grammar::a_star()),
                          ("(a|b)* ∪ (a|c)*d", demo_grammar()),
                          ] {
        println!("grammar for {}", name);
        println!("");
        let g = g.tag_nonterminals();
        let mut rb: RustBackend = Backend::new(&g);
        let mut cg = Codegen { backend: &mut rb, grammar: &g };
        for (i, rule) in g.rules.iter().enumerate() {
            // FIXME: make `fn on_rule` take a `&Rule` instead of cloning.
            let (c, blocks) = cg.on_rule(rule.clone());
            let l_a = cg.backend.nonterm_label(rule.left);
            let b = cg.backend.block(l_a, c);
            let prefix = format!("i={}: ", i);
            println!("{}{}", prefix, b.render());
            let blocks: String = blocks.iter()
                .map(|b|b.render() + "\n")
                .collect();
            println!("{}", blocks);
        }
    }

    // (uncomment to show the output above)
    // assert!(false);
}
