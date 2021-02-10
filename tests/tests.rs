use std::{fs, process::Command};

use fs::File;

#[test]
fn test_queens() {
    test("queens", false);
}

#[test]
fn test_dec2bin() {
    test("dec2bin", false);
}

#[test]
fn test_merge() {
    test("merge", true);
}

fn test(name: &str, has_input: bool) {
    let prog = format!("tests/testcases/{}.tig", name);
    let obj = format!("tests/{}.o", name);
    let exec = format!("tests/{}", name);
    let output = format!("tests/testcases/{}.out", name);

    Command::new("cargo")
        .args(&["run", "--", &prog[..], "-o", &obj[..]])
        .output()
        .unwrap();
    Command::new("cc")
        .args(&["src/tiger.c", &obj[..], "-o", &exec[..]])
        .output()
        .unwrap();

    let actual = if has_input {
        let input = format!("tests/testcases/{}.in", name);
        let input_file = File::open(&input[..]).unwrap();
        Command::new(&exec[..])
            .stdin(input_file)
            .output()
            .unwrap()
            .stdout
    } else {
        Command::new(&exec[..]).output().unwrap().stdout
    };

    Command::new("rm")
        .args(&["-rf", &obj[..]])
        .output()
        .unwrap();
    Command::new("rm")
        .args(&["-rf", &exec[..]])
        .output()
        .unwrap();

    let expected = fs::read(&output[..]).unwrap();
    assert_eq!(actual, expected);
}
