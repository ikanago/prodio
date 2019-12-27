extern crate prodio;

use std::fs::File;
use std::io::{self, Write};
use std::process::{Command, Stdio};

use prodio::compile;

// Unable to execute binary built from assembly.
/*
#[test]
fn test_binary_operator() {
    let code = "(1 + 5 * 4) / 3";
    let generated_code = compile!(code);

    let asm_file_name = "tmp.s";
    let asm_file = File::create(asm_file_name).unwrap();
    let mut writer = io::BufWriter::new(asm_file);
    write!(writer, "{}", generated_code).unwrap();

    use std::env;
    eprintln!("{:?}", env::current_dir().unwrap());
    Command::new("chmod")
        .args(&["755", "tests/test_helper.sh"])
        .spawn()
        .unwrap();
    let mut process = Command::new("sh")
        .arg("./tests/test_helper.sh")
        //        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    //    let process_stdin = process.stdin.as_mut().unwrap();
    //    process_stdin.write_all(asm_file_name.as_bytes()).unwrap();
    let process_output = String::from_utf8(process.wait_with_output().unwrap().stdout).unwrap();
    assert_eq!(process_output, "7".to_string());
}
*/
