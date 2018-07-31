# The Elm Compiler

Learn about the Elm programming language at [elm-lang.org](http://elm-lang.org/).

## WebAssembly explorations

This is a _completely unofficial_ fork of the Elm compiler that targets [WebAssembly](https://webassembly.org/). Elm will very likely compile to WebAssembly some day, but not until the [Garbage Collection extension][gc] is available. When will that be? Dunno. ¯\\\_(ツ)\_/¯

In the meantime, this is a **hobby project** for me. Check out my [blog][blog] where I've started writing posts about it.

[gc]: https://github.com/WebAssembly/gc/blob/master/proposals/gc/Overview.md
[blog]: https://dev.to/briancarroll/elm-functions-in-webassembly-50ak

## Approach & Architecture

- This project is a fork of the official compiler, because I want to reuse the existing parser & optimiser (which are huge) and just focus on code generation.

- I'm generating WebAssembly text format only. This can then be assembled to binary as a separate step using [binaryen](https://github.com/WebAssembly/binaryen).

- The generator is built on top of a few basic modules

  - Types describing the Wasm AST (`compiler/src/Generate/WebAssembly/AST.hs`)
  - A Domain-Specific Language for the full instruction set (`compiler/src/Generate/WebAssembly/Instructions.hs`)
  - A Builder module to convert the AST to text (`compiler/src/Generate/WebAssembly/Builder.hs`)

- The DSL takes full advantage of the text format's nested S-expressions.

  - Nesting makes things _far_ more composable than writing instructions in stack order.
  - Wasm instructions are represented as Haskell functions.
  - The way the DSL works kinda reminds me of the Elm `Html` library (which was definitely an inspiration for it.)

- There is a Garbage _Creator_ included, but not a _collector_ ;)

  - The `GC` module can allocate memory for values and copy them, which is enough to get simple test programs to run.
  - But it can't actually do a _collection_, so when memory fills up, the WebAssembly module will throw an exception and stop.

- Kernel code is _generated_ from Haskell using the instructions DSL.
  - This gives as much control as hand-writing, but with far better flexibility because it's just Haskell functions.
  - For now, the Kernel is in a subdirectory of the code generator. I'll move it when I figure out where to put it!

## Changes made

- Added support for generating `.wat` files as well as `.js` and `.html`. That involved very small changes in several places.
- Added `compiler/src/Generate/WebAssembly.hs` and `compiler/src/Generate/WebAssembly/*`
- Modified the JS code generator to insert comments for every AST node, which helps me to follow what the Haskell code is doing.

## Stuff that doesn't work

- Well... it can't compile a real Elm program!
- For Wasm, it uses a fixed 'test' AST and **totally ignores your source code!!**
  - Implementing `Program` is complicated, and will take some time. And without a valid `main`, everything is 'dead code'.
  - I'll probably try to get it working with a `main` based on static `Html` at some point.
  - For now I have a fake Elm AST to test with, that _looks like_ it came from the previous stage of the compiler. But it's always the same Elm program regardless of the source code you give it!
  - That fake AST is in `Test.hs` [here](https://github.com/brian-carroll/elm-compiler/blob/wasm/compiler/src/Generate/WebAssembly/Test.hs)
