extern crate pgy_runtime;

#[cfg(test)]
use pgy_runtime::graph::Graph;
#[cfg(test)]
use pgy_runtime::graph::Node;

#[cfg(test)]
use self::support::DemoContext;

mod parse_a;
mod support;

#[test]
fn test_a_a() {
    let input = "a";
    let g = Graph::new();
    let mut c = DemoContext::new(input.as_bytes(), &g);
    parse_a::parse(&mut c, parse_a::nonterm::S).unwrap();
}

#[test]
fn test_a_b() {
    let input = "b";
    let g = Graph::new();
    let mut c = DemoContext::new(input.as_bytes(), &g);
    parse_a::parse(&mut c, parse_a::nonterm::S).unwrap_err();
}
