use std::process::Command;

fn main() {
    let output = Command::new("cargo")
        .arg("build")
        .current_dir("examples/micro_grammars")
        .output()
        .unwrap();
    println!("stdout: {}", String::from_utf8_lossy(&output.stdout));
    println!("");
    println!("stderr: {}", String::from_utf8_lossy(&output.stderr));
}
