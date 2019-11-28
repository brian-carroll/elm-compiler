# Elm expressions vs C expressions/statements/declarations

Different possible approaches

## Same as JS generator

- Create a `Code` type and have codeToExpr, codeToBlockItems
- Tricky part is the `return` statement. Different from JS.
- JS generator has a nice symmetry that I don't have in C
  - uses `return` both to return from a function and as part of converting statements to expressions (IIFE)
  - but in C we convert statement to expression using curlies _without_ `return` keyword. Need `return` only at end of function. So we have more syntax permutations to manage.
- don't have a good solution yet

## Everything is a declaration! Create 1 var per subexpression

- Create temp names for all subexpressions, making them into declarations
- Makes everything very uniform, easy to generate. Every recursive call returns the name of its result variable, parent expression is a combination of child variable names.
- C code may be harder to read, less idiomatic.
- But actually easy to debug the compiler!
- Rely on C compiler register allocation to optimise away most of the intermediate variables (experiment with this!)
- **This is nice because it restores 1:1 correspondence between the Elm syntax and the C syntax.** No fancy conversion stuff.
- Do depth-first search, maintain a reverse list of declarations. Deeper calls return a list up to caller. First fold over children and add their decls, then add your own.

## return expr + preceding decls/stmts

- Like the "everything as decl" version but more readable
- Depth first search, folding over child exprs, collecting their pre-decls in a list
- whenever C needs a decl, the return expr is a Var reference.
- whenever the Elm expr is also a C expr, just don't add any pre-decls
- will also need global pre-decls

# TODO 23 Nov 2019

- JS kernel (without deps!)
  - Call `Generate.JavaScript.addGlobal` with the real graph so it can find deps
  - Maintain full JS generation state, not just builders
  - Skip this completely for C kernels because JS has a separate `_seenGlobals` and will take care of itself
- figure out how to deal with `WasmWrapper.element`
  - maybe special-case in `addMain` for top function call
  - maybe make it a kernel function and unlock from elm and elm-explorations, add a wasm thing to the safe list
- expressions!
- output two files instead of one
- generate emscripten wrapper JS code
- maybe get rid of emscripten JS altogether
  - hard part is probably debugging with printf/console.log

# 14 Nov 2019

- C Names module

  - What are all the things I need names for?
    - Go through main.c and find the naming conventions I've used
    - In general go through it and make notes on how to generate everything
  - Make functions to generate each one from AST representation
  - Make it more like JS.Name
  - Rename from CName to Name
  - Name.Name is UTF8

- C module

  - always convert to CN.Name first

- C Builder
  - It's mainly for Expressions really, Nodes can be more low-tech
  - However could put an enum thing here
  - ~~make it about writing C strings from Haskell. No Elm specifics.~~
    - ~~Elm stuff goes in C and Expression~~
  - Always going through C AST results in much fewer micro-decisions and is better type-checked so easier to debug. Make ExternalDeclarations and build those.

# TODO 9 Nov 2019

Figure out a type for the generator state & expr state
express the file structure in types

## JS text

- Async load wasm
- normal Elm stuff
  - functions
  - kernel modules
  - patch
    - jsKernelFunctions array
    - ctor names
    - field names
    - fieldGroupNames
    - wrapWasmElmApp def
    - wrapWasmElmApp call
    - `author$project$WasmWrapper$element` override
  - main export

## JS generation

- can't just call `Generate.JavaScript.generate`, need different top level stuff for async compilation
- copy that function and use the bits of it

## Shared generator state

- Set of seen globals
- Set of JS kernel fn names
- Set of ctors
- Set of record fields
- Set of sets of fields

## JS generator state

- just kernel builders

## C generator state

- header
  - Set of literals
- body
  - builders
- footer
  - List of global names that are GC roots
  - List of global names to be initialised

```hs
data State =
  State
    { _seenGlobals :: Set.Set Opt.Global
    , _revInitGlobals :: [Opt.Global]
    , _revC :: [B.Builder]
    , _revJsKernels :: [B.Builder]
    , _jsKernelVars :: Set.Set ByteString
    , _fieldGroups :: Set.Set [ByteString]
    , _ctorNames :: Set.Set ByteString
    , _literals :: Set.Set ByteString
    }
```

## C text

- top of file
  - includes (kernel + eff mgrs)
  - kernel fn name enum
  - ctor name enum
  - field name enum
  - fieldgroup definitions
  - fieldgroups array
  - literals? `Set Builder`
- global defs
- Wasm module init
  - start boilerplate
    - GC_init & early exit on fail
    - wrapper registrations (fieldgroups & main record)
  - GC roots for Wasm effects (model, then later vdom)
  - inits
  - end boilerplate `return 0`

```cpp
void* thing1;
void* init_thing1() {
  return A2(&Basics_add, &literal_int_1, &literal_int_2);
}
void initialise_global(void** global, void* (*init_func)()) {
  GC_register_root(global);
  if ((*global = init_func()) != pGcFull) return;

  GC_collect_major();
  if ((*global = init_func()) != pGcFull) return;

  fprintf(stderr, "Heap overflow initialising global at %p", global);
  assert(0);
}
int main() {
  initialise_global(&thing1, &init_thing1);
}
```

# TODO 9 Oct 2019

- Working example

  - Git update from master!
  - Pick a program to generate (up/down counter will do)
  - Write the `update` in Elm and C, make a fake AST for it
  - Make the wrapper `Program` to interface to JS runtime
    - will need kernel code => unlock that
  - spit out both JS and C.
  - Integrate it. Probably with some Makefile work

- GC perf

  - see if I can call out to `performance.now()`

- GC completeness

  - fix currying bug
  - more memory

- code gen refactoring

  - fix up the C Builder, still has old stuff in it
  - commit to home grown lib

# TODO 4 Aug 2019

## GC demo

- Long but not infinite counter. Return control to browser occasionally by returning a `Cmd Msg` continuation
- Use C printf to log to console. See if I can call JS perf timer.
- Find some way of comparing to a JS-only version with browser GC
- Find a way of comparing speed?
  - Kinda competing against V8/SpiderMonkey optimisations. Scary.
  - Boxed integers
    - All my Wasm integers are boxed
    - To make it fair, JS version should use boxed integers without `--optimize` and Wasm version should just use Int directly
    - Also measure JS unboxed for completeness, with `--optimize`
    - Compile it twice. Both optimized and unoptimized JS will be racing against the exact same C version.
- Maybe can do this before modifying compiler for C+JS?
  - Use the Bash patching thing I already have, but apply it to different globals. Hand picked ones at first, then maybe pattern-based.
  - Hand compile it.
  - Do some GC stuff in the C version, like with a while loop checking the heap status and deciding whether to clean up.

## Compile a real Elm program

- Write to two output files (.c, .js)
  - what do I have to hack to make this happen?
  - call JS generator from C generator
- Generate C for any global prefixed with `wasm` or in a module beginning with `Wasm` and make it a Wasm export
- Everything else is JS
- Tree traversal
  - separate traversal of C and JS dependencies
  - separate "seen" globals. Some generated for both
- Generate JS to call the Wasm exports
  - Numbers only at first?
  - JSON later

## Implement JSON for imports/exports

- Use that pure Elm JSON parser lib I found somewhere (Ilias?)
- Needs Custom types & some String stuff & UTF

## Tail recursive calls

- enables some good test cases

## Get rid of Language.C

- Leave it in elm.cabal for now to use in GHCi (even needed? check.)

## Local closures, scope, etc.

- language fundamentals!
- Affects expression generator code structure (Elm local -> C global + local)

# Functions code gen

- Global

  - function: definition in global scope
  - Closure: const definition in global scope

- Local
  - function: definition in global scope (needs globally unique name)
  - Closure: allocate & assign

Ok so expression generation should return a tuple
(function def, compound literal)
And surrounding context decides where to assign the compound literal?
Might need to typecast the compound literal in the local case
In `addDef` can separately call internal pieces of function generation and tie them together differently
Local functions are generated in Expression.hs and global ones in C.hs

# Initialization

- What to put in `main` and what to put in the global scope?
- All evaluator function defs in global scope
- All of what code gen calls "globals" must at least be _declared_ globally so code inside functions has them in scope if needed.

- Global scope can have

  - Evaluator functions
  - Closure definitions
  - Constructor definitions
    - with params => evaluator + Closure
    - without params => const

- Can't have
  - Function calls as the expression in an `Opt.Define`
  - Cycles

| Node           | OK in global scope?   | OK in main? |
| -------------- | --------------------- | ----------- |
| Define         | fn or value, not expr | not eval fn |
| DefineTailFunc | Ok                    | not eval fn |
| Ctor           | OK                    | y           |
| Link           | -                     | y           |
| Cycle          | Not OK                | y           |
| Manager        | -                     | y           |
| Kernel         | OK                    | y           |
| Enum           | OK                    | y           |
| Box            | -                     | y           |
| PortIncoming   | OK                    | y           |
| PortOutgoing   | Ok                    | y           |

Approach: two fields in state: one for `main` and one for global scope
Every `Define` at least gets a declaration in global scope but may be initialized in `main`
`main` is really a fallback, only if needed.

# Closing over values

- pull out closed-over values as extra args and curry them in
- need to keep track of scopes as we descend into the AST
- **I did this before in Wasm, find it!**
- There were Sets of vars used, args and locals. Diff the sets to get what's left over, those are outer scope

# Fields

Approaches

1. Generate the fieldset along with the Record constructor

- Difficulty: knowing we're in the constructor
- Need to maintain an AST stack, which is a pain if done everywhere

2. Maintain a set/map of fieldsets, generate them all together at the top

- All records have to start from a full literal. Elm has no way to add fields that weren't already there.
- Whenever you generate a Record literal
  - Get the List of its fields and wrap it in a `Set.singleton`
  - Take the union of that set with the `Set [Name]` in the generator state
  - `union` is efficient, doesn't do an update if already there

# Constructor tags

Approaches

1. Global enum at the top

- no need for globally unique names, don't care about name clashes between separate types since Elm compiler keeps them apart

2. Constants next to constructor function, set equal to `index` from compiler

- downside: need globally unique names
- definition of ctor ID is next to constructor function. Meh.

3. Just use integers like JS prod mode

- downside: lack of readability with no improvement over C enum
