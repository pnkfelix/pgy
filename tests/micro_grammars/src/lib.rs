extern crate pgy_runtime;

pub mod parse_a;

#[cfg(test)]
mod test_a {
    use pgy_runtime::graph::Graph;
    use pgy_runtime::demo::{DemoContext, ParseError, Success};
    use super::parse_a::{parse, nonterm};

    fn demo(input: &str) -> Result<Success, ParseError> {
        let g = Graph::new();
        let mut c = DemoContext::new(input.as_bytes(), &g);
        parse(&mut c, nonterm::S)
    }

    #[test]
    fn test_a() {
        demo("a").unwrap();
    }

    #[test]
    fn test_b() {
        demo("b").unwrap_err();
    }
}

#[allow(non_snake_case)]
pub mod parse_S;

#[allow(non_snake_case)]
#[cfg(test)]
mod test_S {
    use pgy_runtime::graph::Graph;
    use pgy_runtime::demo::{DemoContext, ParseError, Success};
    use super::parse_S::{parse, nonterm};

    fn demo(input: &str) -> Result<Success, ParseError> {
        let g = Graph::new();
        let mut c = DemoContext::new(input.as_bytes(), &g);
        parse(&mut c, nonterm::S)
    }

    #[test]
    fn test_a() {
        // S -> B S -> a S -> a
        demo("a").unwrap();
    }

    #[test]
    fn demo_ad() {
        // S -> A S d ->* a d
        demo("ad").unwrap();
    }

    #[test]
    fn demo_aadd() {
        // S -> A S d -> A A S d d ->* a a d d
        demo("aadd").unwrap();
    }

    #[test]
    fn demo_acdd() {
        // S -> A S d -> A A S d d ->* a c d d
        demo("acdd").unwrap();
    }

    #[test]
    fn demo_cadd() {
        // S -> A S d -> A A S d d ->* c a d d
        demo("cadd").unwrap();
    }

    #[test]
    fn demo_b() {
        // S -> B S ->* b
        demo("b").unwrap();
    }

    #[test]
    fn demo_bb() {
        // S -> B S -> B B S ->* b b
        demo("bb").unwrap();
    }

    #[test]
    fn demo_bbb() {
        // S -> B S -> B B S -> B B B S ->* b b b
        demo("bbb").unwrap();
    }

    #[test]
    fn demo_bad() {
        // S -> B S -> B A S d ->* b a d
        demo("bad").unwrap();
    }

    #[test]
    fn demo_bcd() {
        // S -> B S -> B A S d ->* b c d
        demo("bcd").unwrap();
    }

    #[test]
    fn demo_bbad() {
        // S -> B S -> B B S -> B B A S d ->* b b a d
        demo("bbad").unwrap();
    }

    #[test]
    fn demo_bbcd() {
        // S -> B S -> B B S -> B B A S d ->* b b c d
        demo("bbcd").unwrap();
    }
    #[test]
    fn demo_abd() {
        // S -> A S d -> A B S d ->* a b d
        demo("abd").unwrap();
    }
    #[test]
    fn demo_cbd() {
        // S -> A S d -> A B S d ->* c b d
        demo("cbd").unwrap();
    }
    #[test]
    fn demo_bacdd() {
        // S -> B S -> B A S d -> B A A S d d ->* b a c d d
        demo("bacdd").unwrap();
    }
    #[test]
    fn demo_bcadd() {
        // S -> B S -> B A S d -> B A A S d d ->* b c a d d
        demo("bcadd").unwrap();
    }
    #[test]
    fn demo_bbcadd() {
        // S -> B S -> B B S -> B B A S d -> B B A A S d d ->* b b c a d d
        demo("bbcadd").unwrap();
    }
    #[test]
    fn demo_bbacdd() {
        // S -> B S -> B B S -> B B A S d -> B B A A S d d ->* b b a c d d
        demo("bbacdd").unwrap();
    }
    #[test]
    fn demo_bbaacddd() {
        // S ->* B B A A S d d -> B B A A A S d d d ->* b b a a c d d d
        demo("bbaacddd").unwrap();
    }
    #[test]
    fn demo_bbaccddd() {
        // S ->* B B A A S d d -> B B A A A S d d d ->* b b a c c d d d
        demo("bbaccddd").unwrap();
    }
    #[test]
    fn demo_bbcacddd() {
        // S ->* B B A A S d d -> B B A A A S d d d ->* b b c a c d d d
        demo("bbcacddd").unwrap();
    }

    #[test]
    fn demo_errs() {
        demo("c").unwrap_err();
        demo("d").unwrap_err();
        demo("bbc").unwrap_err();
    }

    #[test]
    fn demo_5() {
        demo("").unwrap();
        demo("abcacddd").unwrap();
    }
}
