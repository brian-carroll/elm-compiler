# Elm compiler with C code generation

- This fork of the Elm compiler generates C code that can be further compiled to WebAssembly using Emscripten.
- It also generates a JavaScript file to wrap the WebAssembly module and connect it to the Elm runtime.
- The Elm runtime is implemented in JavaScript so that it can access browser APIs, which are not currently available in WebAssembly.
- For more detail on this architecture, see the [JS wrapper docs](./wrapper.md)

## Changes to the CLI

- [terminal/src/Make.hs](https://github.com/brian-carroll/elm-compiler/blob/master/terminal/src/Make.hs)
- Create an output mode for C `--output foo.c`
  - The official compiler already detects `*.js` and `*.html` file extensions, so add `*.c` to that list.
- When a C output is specified, write _two_ output files, `foo.c` and `foo.js`. The JS filename is inferred from the C filename.

## Changes to code generation

- [builder/src/Generate/](https://github.com/brian-carroll/elm-compiler/tree/master/compiler/src/Generate)
- Add an extra submodule `C.hs` to the Generate module, which returns a pair of strings to be written to the two output files.
- Expose some functions from the JS code generator that we need to call from the C generator
- Add comments to the JS code. This was helpful during development to analyse exactly how the AST maps to JS.

# C file structure

![Diagram of C file structure](./docs/images/c-file-structure.png)


## Constant declarations

- Constructor IDs
- Record field IDs & field groups
- JS kernel function IDs & closures
- Program constants (string literals, etc.)

## Elm top level values

- Values defined in the top level scope of any Elm module
- They are also defined in the top level scope of the C program ("external declarations")
- Most of them are named functions, though they can be any value
- The compiler sometimes uses several aliases for the same value. The `#define` directive is a great way to do this in C.
- Initialised top level values
  - A small number of top level values are expressions that need to be evaluated when the program is loaded.
  - In JS, this happens when the JS engine loads the file.
  - In C, we are not allowed to have non-constant expressions in the top-level scope, unlike in Elm and JS. We need to wrap each non-constant expression in a C function, to be called exactly once from the C `main` function.
  - If a top-level value is initialised then it will be allocated on the heap instead of being considered a constant. It needs to be registered as a GC root, and the generated code ends up being a bit more complicated.

```elm
needToEvaluateBeforeAppInit : String
needToEvaluateBeforeAppInit = "Hello " ++ "world"
```

## Elm `main` values

- The compiler supports multiple `main` values in the same output bundle. They are generated as a C array, which is passed via the wrapper to the JS kernel.
- All of the code is in place to support having multiple `main`s. (It was actually easier to keep this feature than to remove it!) But it's completely untested, so it may not be working yet.

## C `main` function

- Emscripten calls the C `main` function after the WebAssembly module is loaded
- Initialise GC
- Call any initialisation functions for top level values, in dependency order. Allocate them on the heap and register them as GC roots.
- Initialise the JS/Wasm wrapper. (In hindsight, this step could be eliminated.)
