use std::{fs, process::Command};

#[test]
fn test_tigerc() {
    Command::new("cargo").args(&[
        "run",
        "--",
        "tests/testcases/queens.tig",
        "-o",
        "tests/queens.o",
    ]).output().unwrap();
    Command::new("cc").args(&[
        "src/tiger.c",
        "tests/queens.o",
        "-o",
        "tests/queens",
    ]).output().unwrap();
    let actual = Command::new("tests/queens").output().unwrap().stdout;
    Command::new("rm").args(&[
        "-rf",
        "tests/queens.o",
    ]).output().unwrap();
    Command::new("rm").args(&[
        "-rf",
        "tests/queens",
    ]).output().unwrap();
    
    let expected = fs::read("tests/testcases/queens.out").unwrap();
    assert_eq!(actual, expected);
}
