# Prodio

![Rust](https://github.com/ikanago/prodio/workflows/Rust/badge.svg)

## What's this🤔
This is a new programming language written in Rust. "Prodio" means "advance" in Latin.  
I'm not going to develop a practical language, but I plan to implement a lot of functionalities I've never implemented.

## Usage💻
How to compile and run C source code:
```bash
./run.sh <CODE(*.pro)>
```
Then return code is prompted as a result of the program.

## Contents⚙
* arithmetical calculation
* variable
* `if` statement

## Compile Path🛠
```
Tokenize(lexer.rs)
        ↓
Parse(parser.rs)
        ↓
IR Generation(gen_ir.rs)
        ↓
Code Generation(gen_code.rs)
```