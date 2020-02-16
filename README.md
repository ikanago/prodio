# Prodio

![Rust](https://github.com/ikanago/prodio/workflows/Rust/badge.svg)

## What's this🤔
This is a C compiler written in Rust. "Prodio" means "advance" in Latin.

I'm developing this compiler as a rebuild of ycc, C compiler written in C(refer to my Repositories). I want to create advanced compiler compared to ycc in functionality and readability.

## Usage💻
How to compile and run C source code:
```bash
./run.sh <CODE(*.c)>
```
Then return code is prompted as a result of the program.

## Contents⚙
* arithmetical calculation
* variable

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