[![CircleCI](https://circleci.com/gh/Technius/simpl.svg?style=svg)](https://circleci.com/gh/Technius/simpl)

# simpl-lang

`SimPL` is a **s**imple **im**perative **p**rogramming **l**anguage inspired by
functional programming. It comes with a compiler that uses LLVM to generate
native machine code.

See [this sample file](sample.spl) for a brief demonstration example of the
syntax. For more comprehensive examples, check out the [test suite](test-suite).

Current language features:

* Integers and floats, with explicit numeric conversions
* Strings
* Algebraic data types: sum types, product types
* Functions and function application
* Function references (to static functions only right now)
* Static types with type inference
* Rank 1 parametric polymorphism

## Compiling

LLVM 9 and clang 9 must be installed.

To compile, run `stack build`. When developing, use the `--fast` flag.

The test suite programs can be compiled with `make test` and the generated
executables can be found in `test-suite/bin`.

## Usage

The SimPL runtime currently uses the
[`boehm-gc`](https://github.com/ivmai/bdwgc) garbage collector to handle heap
allocations. When linking object files created by `simplc`, make sure that you
link `libgc` as well.

Run the compiler using
```bash
stack run simplc -- <source file> -o <target file>
```

To debug the compiler, use one or more of the `--dump-joinir` and `--dump-ir`
flags.

## License

Copyright 2018-2019 Bryan Tan ("Technius")

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

