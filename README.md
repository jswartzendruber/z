# Building
```
mkdir build
cd build
cmake -G Ninja ..
cmake --build .
```

# Usage
```
./z examples/fib.z # compile fib.z
./examples/fib.z.out # run compiled program
fib(35) = 9227465 # example output
```

# Dependencies
gcc must be available in the PATH.

# Overview
This C-like language uses a recursive descent parser and lexer to construct an
AST, uses the visitor pattern to traverse the AST and typecheck, then walks the
AST to produce equivalent C code. The C code is saved to a file alongside the z
code, then gcc compiles the C code producing an executable .out file.