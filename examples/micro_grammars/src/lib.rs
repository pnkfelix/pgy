extern crate pgy_runtime;

#[cfg(test)]
use pgy_runtime::graph::Graph;
#[cfg(test)]
use pgy_runtime::graph::Node;

#[cfg(test)]
use self::support::DemoContext;

mod parse_a;
mod support;

mod test_a {
    use pgy_runtime::graph::Graph;
    use super::support::{DemoContext, ParseError, Success};
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

mod parse_S;
mod test_S {
    use pgy_runtime::graph::Graph;
    use super::support::{DemoContext, ParseError, Success};
    use super::parse_S::{parse, nonterm};

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
    fn test_ad() {
        demo("ad").unwrap();
    }

}
