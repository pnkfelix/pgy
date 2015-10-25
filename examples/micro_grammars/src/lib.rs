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
    use super::support::DemoContext;
    use super::parse_a::{parse, nonterm};

    #[test]
    fn test_a_a() {
        let input = "a";
        let g = Graph::new();
        let mut c = DemoContext::new(input.as_bytes(), &g);
        parse(&mut c, nonterm::S).unwrap();
    }

    #[test]
    fn test_a_b() {
        let input = "b";
        let g = Graph::new();
        let mut c = DemoContext::new(input.as_bytes(), &g);
        parse(&mut c, nonterm::S).unwrap_err();
    }
}

mod parse_S;
mod test_S {
    use pgy_runtime::graph::Graph;
    use super::support::DemoContext;
    use super::parse_S::{parse, nonterm};

    #[test]
    fn test_a_a() {
        let input = "a";
        let g = Graph::new();
        let mut c = DemoContext::new(input.as_bytes(), &g);
        parse(&mut c, nonterm::S).unwrap();
    }

}
