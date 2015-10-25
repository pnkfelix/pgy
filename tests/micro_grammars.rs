use std::process::{Command, exit};

fn main() {
    let output = Command::new("cargo")
        .arg("test")
        .current_dir("tests/micro_grammars")
        .output()
        .unwrap();
    println!("stdout: {}", String::from_utf8_lossy(&output.stdout));
    println!("");
    println!("stderr: {}", String::from_utf8_lossy(&output.stderr));
    exit(output.status.code().unwrap_or(1));
}
