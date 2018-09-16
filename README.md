# simpl-lang

`SimPL` is a **s**imple **im**perative **p**rogramming **l**anguage inspired by
functional programming. It compiles to LLVM.

## Compiling

LLVM `6.0.1` must be installed. Note: `llvm-hs` may fail to compile the C++
files due to a name ambiguity bug in the LLVM header file
`llvm/ExecutionEngine/Orc/LambdaResolver.h`. See
[this](https://gist.github.com/yuhangwang/380b94bebe2af386fea9d2e352dbae52) for
one potential fix.

To compile, run `stack build`. When developing, use the `--fast` flag. A
`test.sh` script is provided to quickly test compilation.

## License

Copyright 2018 Bryan Tan ("Technius")

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

