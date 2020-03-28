# Elm compiler with C code generation

This fork of the Elm compiler generates C code that can be further compiled to WebAssembly using Emscripten.

It also generates a JavaScript file to wrap the WebAssembly module and connect it to the Elm runtime. The Elm runtime is implemented in JavaScript so that it can access browser APIs, which are not currently available in WebAssembly.

For more detail on this architecture, see the [JS wrapper docs](./wrapper.md)

## Changes to the CLI

[terminal/src/Make.hs](https://github.com/brian-carroll/elm-compiler/blob/master/terminal/src/Make.hs)
- Create an output mode for C `--output foo.c`
  - The official compiler already detects `*.js` and `*.html` file extensions, so add `*.c` to that list.
- When a C output is specified, write _two_ output files, `foo.c` and `foo.js`. The JS filename is inferred from the C filename.

## Changes to code generation

[builder/src/Generate/](https://github.com/brian-carroll/elm-compiler/tree/master/compiler/src/Generate)
- Add an extra submodule `C.hs` to the Generate module, which returns a pair of strings to be written to the two output files.
- Expose some functions from the JS code generator that we need to call from the C generator
- Add comments to the JS code. This was helpful during development to analyse exactly how the AST maps to JS.

## C file structure

![Diagram of C file structure](./docs/images/c-file-structure.png)


### Constant declarations

- Program constants (string literals, etc.)
- Custom type constructor IDs
- Record type info (field IDs & "field groups")
- JS kernel function IDs & closures

### Elm top level values

- Values defined in the top level scope of any Elm module
  - They're mostly functions, but can be other things too
- They compile to declarations in the top level scope of the C program ("external declarations")
- The Elm compiler sometimes uses several aliases for the same value, and this is implemented in C using the `#define` directive
- Initialised top level values
  - A small number of top level values are expressions that need to be evaluated when the program is loaded.
  - In JS, this happens when the JS engine loads the file.
  - In C, we are not allowed to have non-constant expressions in the top-level scope, unlike in Elm and JS. We need to wrap each non-constant expression in a C function, to be called exactly once from the C `main` function.
  - If a top-level value is initialised then it will be allocated on the heap instead of being considered a constant. It needs to be registered as a GC root, and the generated code ends up being a bit more complicated.

```elm
needToEvaluateBeforeAppInit : String
needToEvaluateBeforeAppInit = "Hello " ++ "world"
```

### Elm `main` values

- The compiler supports multiple `main` values in the same output bundle. They are generated as a C array, which is passed via the wrapper to the JS kernel.
- All of the code is in place to support having multiple `main`s. (It was actually easier to keep this feature than to remove it!) But it's completely untested, so it may not be working yet.

### C `main` function

- Emscripten calls the C `main` function after the WebAssembly module is loaded
- Initialise GC
- Call any initialisation functions for top level values, in dependency order. Allocate them on the heap and register them as GC roots.
- Initialise the JS/Wasm wrapper. (This step could be eliminated in a future refactor.)

## C code style

An example of generated C code can be found [here](https://github.com/brian-carroll/elm-compiler/blob/master/test/cycle/hand_edited.c). That example is very likely to get outdated, but might be interesting to browse through to get an initial feel for things.

Here are a few points to note about it

- All Elm values are accessed via pointers.
- All dynamic values are allocated on the heap. We don't use the stack very much.
  - Heap allocation is extremely cheap with the GC we're using. It's just a matter of incrementing a pointer. You may have been taught that allocation is far cheaper on the stack than the heap, but that advice tends to assume you're using `malloc` which we're not.
- Constants are not allocated on the heap. They are stored as part of the program data.
  - This includies language built-ins like `True`, `False`, `()` and `[]`, as well as program-specific constants like string and number literals from the Elm source.
  - Constants are accessed via pointers, the same as dynamically allocated values. They're just stored in a different address range.
  - In most cases it would be nice if `True` and `False` were just represented as integers `1` and `0` instead of pointers to constant structures. But that would make it more complicated to implement a `List Bool`, for example. It's simpler for now if everything is always a pointer.
- Elm names are prefixed with `x_`, so that the Elm name `model` becomes the C name `x_model`. This is to help ensure that any compiler-generated names can't clash with user-defined names. The JS code generator achieves the same thing by prefixing compiler-generated names with an underscore, but that approach is more dangerous in C. There are a lot of hidden magic symbols that begin with underscores.
- Macros like `A1`, `A2`, `A3` are used for function application, just like in the JavaScript code generator. These are C preprocessor macros defined in the [C implementation of the Elm core libraries](https://github.com/brian-carroll/elm_c_wasm/blob/fa096c3516fafdcc88c2047744dc686e05cd3cd2/src/kernel/utils.h)
