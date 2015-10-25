#[macro_use(rule)]
extern crate pgy;

use std::io::{self, BufWriter, Write};
use std::fs::File;

use pgy::grammar::{Grammar, Rule};
use pgy::codegen::rust::RustBackend;
use pgy::gll::codegen::codegen;

fn main() {
    let g = Grammar::new(vec![rule!(S ::= ('a'))]);
    generate(g, "_a");

    let g = Grammar::new(vec![rule!(S ::= ("A" "S" 'd'); ("B" "S"); ()),
                              rule!(A ::= ('a'); ('c')),
                              rule!(B ::= ('a'); ('b'))]);
    generate(g, "_S");
}

fn generate(g: Grammar, suffix: &str) {
    let g = g.tag_nonterminals();
    let mut rb = RustBackend(&g);
    let code = codegen(&mut rb);
    let file = format!("src/parse{}.rs", suffix);
    write_code(code, &file).unwrap()
}

fn write_code(code: String, file: &str) -> io::Result<()> {
    let f = try!(File::create(file));
    let mut w = BufWriter::new(f);
    write!(w, "{}", code)
}
