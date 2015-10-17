

use grammar::demo_grammar;
use codegen::rust::RustBackend;
use gll::codegen::{Backend, Codegen};

#[test]
fn demo_1() {
    let g = demo_grammar();
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
        for _ in 0..prefix.len() {
            print!(" ");
        }
        let blocks: String = blocks.iter()
            .map(|b|b.render() + "\n")
            .collect();
        println!("{}", blocks);
    }

    // FIXME: something seems off about the code-generation,
    // in particular I find this suspect: *---------------+
    //                 //                                 |
    // L::R_A_1 => {   //                                 |
    //     self.pop(); // <--- what is this doing here? --+
    //     if self.i_in(&['a','b','c','d',]) {
    //         self.create(L::R_S_1);
    //         goto!( L::L_S );
    //     } else {
    //         goto!( L::_0 );
    //     }
    //     if self.i_in(&['d']) {
    //         self.i.incr();
    //     } else {
    //         goto!( L::_0 );
    //     }
    //     goto!( L::_0 );
    // }


    // (uncomment to show the output above)
    // assert!(false);
}
