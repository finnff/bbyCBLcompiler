# bbycblgo- A babycbl Compiler/Parser in Go

## Background

Started of as a fork/rewrite of our original python based project [https://github.com/SilasGitHub/software-evolution](https://github.com/SilasGitHub/software-evolution), but due to shitty performance when parsing a large amount of test cases in python (even after implementing multiprocessing, seemed to be core langauage limitations, with slow file IO and [Global Interpreter Lock (GIL)](https://realpython.com/python-gil/)), I thankfully decided to rewrite the Parser/Tester/compiler in Go. 


<img width="510" height="463" alt="gorewrite" src="https://github.com/user-attachments/assets/97d556cd-068a-4ffe-8f71-509d6b471b45" />


## Librays/Tools/Bindings used:
* ANTLR4 (Go runtime): Takes our bbyCBL.g4 grammar and generates a lexer/parser to process babyCOBOL code into an AST
* [TinyGo LLVM Bindings](https://github.com/tinygo-org/go-llvm) (`tinygo.org/x/go-llvm`): Provides the Go language bindings to interact with the LLVM compiler infrastructure, enabling the generation of LLVM IR. Using these as the [official LLVM Go bindings](https://pkg.go.dev/github.com/axw/gollvm) haven't been updated in 10+ years. 
* LLVM: Handles optimization and machine code generation from our IR output. This project is loosely based on [LLVM's Kaleidoscope tutorial](https://llvm.org/docs/tutorial/)


## Features

How It Works:

1. **Feed it babyCOBOL**: Start with your .baby files 

2. **Preprocessing**: Run through `preprocessCobolAdvanced` to handle babyCOBOL's syntax peculiarities

3. **Lexing & Parsing**: 
   * ANTLR4 lexer breaks everything into tokens
   * Parser builds an AST from those tokens

4. **Semantic Analysis (Two-Pass Process)**:
   * **Pass 1**: `AlterCollector` visitor identifies ALTER statements and GO TO targets (to support babyCOBOL's dynamic jumps), builds symbol table
   * **Pass 2**: Full semantic analysis - type checking, variable resolution, validation

5. **Code Generation**: 
   * `CodeGenerator` walks the validated AST
   * Uses TinyGo LLVM bindings to translate AST nodes into LLVM IR (`.ll` files)
   * Handles memory allocation, control flow, arithmetic operations

6. **Output**: The LLVM IR is then invoked by llvm's CompileIRToExecutable which compiles, links it and optimizes it into a binary executable (host architecture dependent)

## Usage and Pre-requisites

* **Requirements:** Install `clang-devel` and `llvm-dev` packages to provide the required headers for TinyGo LLVM bindings.
* The Makefile will automatically detect and select your installed LLVM version when running `make build`.
* For detailed usage see [`./bbycblgo_compiler/README.md`](./bbycblgo_compiler/README.md)*


## Compatibility
Tested on:
- **Fedora** with Go 1.23.1, LLVM 20
- **Ubuntu 24.04 LTS** with LLVM 18.1.3



## Repo Overview
- [`./bbycblgo_compiler/`](./bbycblgo_compiler/) - Main compiler code
- [`./bbycblgo_compiler/handmade/`](./bbycblgo_compiler/handmade/) - Handwritten .baby test cases with corresponding .ll IR, x86_64 binaries, and .ast files
- [`./bbycblgo_compiler/handmade/README.md`](./bbycblgo_compiler/handmade/README.md) - Checkpoint XPs and related test cases
- [`./referenceDocs/`](./referenceDocs/) - Grep-able siterip of [https://slebok.github.io/baby/](https://slebok.github.io/baby/)
- [`./helperscripts/`](./helperscripts/) - .ll disassembler script and LLVM-golang binding verification script


## Screenshots

#### Test Case Coverage 

<img width="787" height="368" alt="image" src="https://github.com/user-attachments/assets/c28feff5-131b-4b31-b8a3-d34eed5b7126" />

#### compile --verbose test11.baby 

<img width="775" height="998" alt="image" src="https://github.com/user-attachments/assets/ff8b15da-3655-4348-a71a-18ee12287384" />

#### ast test32.baby

<img width="641" height="977" alt="image" src="https://github.com/user-attachments/assets/b8f0ff2a-e0d6-41c3-a9a8-c464d11b30ef" />

#### test --testdir handmade -pp , ast test02.baby, and compiled fizzbuzz binary output.

<img width="838" height="1205" alt="image" src="https://github.com/user-attachments/assets/92a4f5d1-b06c-486b-890b-716ca40b04f6" />






