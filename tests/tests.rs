use std::{fs, process::Command};

#[test]
fn test_queens() {
    test("queens");
}

#[test]
fn test_dec2bin() {
    test("dec2bin");
}

fn test(name: &str) {
    let prog = format!("tests/testcases/{}.tig", name);
    let obj = format!("tests/{}.o", name);
    let exec = format!("tests/{}", name);
    let out = format!("tests/testcases/{}.out", name);

    Command::new("cargo").args(&[
        "run",
        "--",
        &prog[..],
        "-o",
        &obj[..],
    ]).output().unwrap();
    Command::new("cc").args(&[
        "src/tiger.c",
        &obj[..],
        "-o",
        &exec[..],
    ]).output().unwrap();

    let actual = Command::new(&exec[..]).output().unwrap().stdout;

    Command::new("rm").args(&[
        "-rf",
        &obj[..],
    ]).output().unwrap();
    Command::new("rm").args(&[
        "-rf",
        &exec[..],
    ]).output().unwrap();

    let expected = fs::read(&out[..]).unwrap();
    assert_eq!(actual, expected);
}
