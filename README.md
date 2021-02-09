# tigerc-rs
This project is a compiler of a toy language implemented in Rust, using [Cranelift](https://github.com/bytecodealliance/wasmtime/tree/main/cranelift).

The toy language, named Tiger, is introduced in the book Modern Compiler Implementation in C.

The compiler generates x86_64 ELF files.

## Usage
Compile the Tiger program into a relocatable object file with `cargo run` and then link it with src/tiger.c to get the executable object file.
```
cargo run -- prog.tig -o prog.o
cc src/tiger.c prog.o -o prog
```
