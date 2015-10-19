extern crate pgy_runtime;

use pgy_runtime::graph::{Graph, Node};
use self::support::DemoContext;

mod parse_a;
mod support;

#[test]
fn test_a() {
    let input = "a";
    let g = Graph::new();
    let mut c = DemoContext::new(input.as_bytes(), &g);
    parse_a::parse(&mut c, "S").unwrap();
}
