use std::process::{Command, exit};

fn main() {
   let output = Command::new("cargo")
        .arg("update")
        .current_dir("tests/micro_grammars")
        .output()
        .unwrap();
    println!("cargo update stdout: {}", String::from_utf8_lossy(&output.stdout));
    println!("");
    println!("cargo update stderr: {}", String::from_utf8_lossy(&output.stderr));
    if !output.status.success() {
        exit(output.status.code().unwrap_or(1));
    }

    let output = Command::new("cargo")
        .arg("test")
        .current_dir("tests/micro_grammars")
        .output()
        .unwrap();
    println!("cargo test stdout: {}", String::from_utf8_lossy(&output.stdout));
    println!("");
    println!("cargo test stderr: {}", String::from_utf8_lossy(&output.stderr));
    exit(output.status.code().unwrap_or(1));
}
