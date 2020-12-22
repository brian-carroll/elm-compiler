# Unboxed ints

| Opt AST        | need more AST type info for unboxing? |
| -------------- | ------------------------------------- |
| Bool           | No (obvious)                          |
| Chr            | No (obvious)                          |
| Str            | No (obvious)                          |
| Int            | No (obvious)                          |
| Float          | No (obvious)                          |
| VarLocal       | _YES_ (supertype specialization)      |
| VarGlobal      | _YES_ (supertype specialization)      |
| VarEnum        | No                                    |
| VarBox         | No                                    |
| VarCycle       | _YES_ (supertype specialization)      |
| VarDebug       | No                                    |
| VarKernel      | No                                    |
| List           | _YES_ (header flags)                  |
| Function       | _YES_ (header flags)                  |
| Call           | No                                    |
| TailCall       | No                                    |
| If             | No                                    |
| Let            | No                                    |
| Destruct       | No (does not dereference inner value) |
| Case           | No (enough info already)              |
| Accessor       | No (does not dereference field value) |
| Access         | No (does not dereference field value) |
| Update         | No (does not dereference field value) |
| Record         | _YES_ (header flags)                  |
| Unit           | No                                    |
| Tuple          | _YES_ (header flags)                  |
| Shader         | No                                    |
| ---------      | --------------------------------      |
| Define         | No                                    |
| DefineTailFunc | No                                    |
| Ctor           | _YES_ (header flags)                  |
| Enum           | No (obvious)                          |
| Box            | No                                    |
| Link           | No                                    |
| Cycle          | No                                    |
| Manager        | No                                    |
| Kernel         | No                                    |
| PortIncoming   | No                                    |
| PortOutgoing   | No                                    |

# elm.js analysis

- Using my generated JS code comments with AST info
- The Ctor node appears in AST as a `Call` to a `VarGlobal`
- Code gen would need to keep a Map of VarGlobals with their bit-masks
- Also needs to map VarGlobal to Functions too

## Compiler code analysis

```hs
compile :: Pkg.Name -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> Either E.Error Artifacts
compile pkg ifaces modul =
  do  canonical   <- canonicalize pkg ifaces modul
      annotations <- typeCheck modul canonical
      ()          <- nitpick canonical
      objects     <- optimize modul annotations canonical
      return (Artifacts canonical annotations objects)
```

- I printed out values (creating Show instances of all the types, ugh)
- `annotations` is a Map of top level names and their types
  - The types of deeply-nested nodes are _thrown away_ during the type solve
  - It doesn't modify the AST at all, just validates it and maybe reports errors
    - But for unboxing, we would need to return a modified AST from typeCheck
- nitpick does some extra error checks and stuff, don't care
- Then `Compile.optimize` just reduces the graph down to min size
  - `objects :: Opt.LocalGraph` is saved to an .elmo object file

So how to get type solver to gives us info for unboxing?

- `Type.Constrain.Module.constrain` takes Canonical AST and returns a `Constraint` tree
- `Type.Solve.run` takes Constraints, returns error or `Map Name Annotation`
- Constraint is defined in Type.Type

## Analysis of Constraint type

- Tree that follows the structure of the AST but is separate, with no references to AST
  - It doesn't need to know the difference between `x + 5` and `x - 3`
- Contains `Variable` which is a mutable value `IORef`
- Possible approach
  - For each of the AST nodes where we need bitflags:
    - When creating the AST, insert a unique type `Variable` using `mkFlexVar`
    - When generating constraints from AST, add a `CEqual` to say the AST node's type has to match its type `Variable`
  - Then let the solver figure out what it is and write it using mutation (Yikes!)
  - When converting to Optimized AST, cut it down to [BoxedOrUnboxed]
  - Encode [BoxedOrUnboxed] to Binary as bit flags

## Basics functions/ops that need to be specialised int/float

```
+, -, *, ^, ==, /=, <, >, <=, >=
max, min, compare, negate, abs, clamp
```

There are about twice as many that don't need specialisation
I wonder if we could literally change the function name at some earlier stage in the compiler, like `Optimize`

## Problems with unboxing supertype calls?

What do we do about unboxing for calls that genuinely have `number` type?

```elm
contrivedExampleAdder : number -> number -> number
contrivedExampleAdder a b =
  a + b -- this inner `Call` node never gets a concrete type. Do we unbox it?
```

Do we have to monomorphise?
Same issue occurs with `comparable`

Horrible ideas:

- Do we need to convert between boxed and unboxed Ints in the final program?! Ugh!!
- Try to deref and see if it looks like a Float header?? Disastrous edge-case errors!

Correct idea:

- Do something like how Haskell does type classes!
- Insert a function param for each type var. Elm should only need a number, not a dict.
- The top-level call site will always be concrete, so we can pass down the values of each type var from there
- Code gen needs to keep a Map of type vars by name in the current scope, to pass them down correctly

This probably solves the issue with binops neatly as well!

Code gen for my contrived example

```c
Closure Basics_add;
void* contrivedExampleAdder(void* args[]) {
  void* number = args[0]; // not actually a pointer, just looks like one
  void* a = args[1];
  void* b = args[2];
  void* result;
  result = A3(&Basics_add, number, a, b);
  return result;
}
void* eval_Basics_add(void* args[]) {
  // At the bottom of the call stack we will always get to some kernel function
  Tag number = * (Tag*) args[0];
  void* a = args[1];
  void* b = args[2];
  void* result;
  if (number == Tag_Int) {
    result = (void*)((size_t)a + (size_t)b)
  } else {
    result = ((ElmFloat*)a)->value + ((ElmFloat*)b)->value;
  }
  return result;
}
```

### What about partially-applied functions with a typevar in the _next_ arg?

type var arg needs to go just before the first arg that uses it!

### What about a type var that equals a Custom type with Ints and Strings and Floats in it?

Does type var argument needs to be a pointer to some data structure?!
Do we need to generate code to pull the sub-variables out of that data structure for subsequent calls?! Aaagh!
No, hang on...
**Elm can't do this! Yay!**
A function body can't assume that type var `a` is a Custom or whatever and then extract another type variable out of it.
That would require typeclasses or higher kinded polymorphism or something
In Elm if an arg contains a general type variable then you can't destructure/dereference it at any lower level in the call stack
**We only need to care about supertypes.**
Phew!
This reduces the amount of data we need.

For each supertype in the function signature, we need to know for each call site:

- Which is the first arg to use it?
- What is its value? ... OR ... Is it boxed or unboxed?

```hs
-- only what we need for unboxing
data UnboxedSuperTypePositions
  = NoUnboxedSuperTypes
  | UnboxedNum Int              -- first position where number=Int
  | UnboxedCompInt Int          -- first position where comparable=Int
  | UnboxedCompChar Int         -- first position where comparable=Char
  | UnboxedNumCompInt Int Int   -- first positions where number=Int & comparable=Int
  | UnboxedNumCompChar Int Int  -- first positions where number=Int & comparable=Char

-- more general, more opportunities for good code gen
-- only costs an extra byte compared to above, and only in some cases
-- could condense even more with loads of constructors,
--    but it would be annoying and only save a tiny amount of data in real programs
data CallSuperTypePositions
  = NoSuperTypes
  | Number Int SuperTypeMember -- explicitly say int or float
  | Comp Int SuperTypeMember
  | Append Int SuperTypeMember
  | NumberAndComp Int Int SuperTypeMember SuperTypeMember
  | CompAndAppend Int Int SuperTypeMember SuperTypeMember
  | NumberAndAppend Int Int SuperTypeMember SuperTypeMember
  | NumCompAppend Int Int Int SuperTypeMember SuperTypeMember SuperTypeMember

data SuperTypeMember
  = SuperInt
  | SuperFloat
  | SuperChar
  | SuperString
  | SuperList
  | SuperTuple2
  | SuperTuple3
```

### deeper and deeper...

Does it even make sense to associate this supertype stuff with a call site?

```elm
applyIntBinop : (Int -> Int -> Int) -> Int -> Int -> Int
applyIntBinop binop a b =
  binop a b -- at this callsite we don't even know if binop was originally polymorphic

op : Bool -> (Int -> Int -> Int)
op doMult = if doMult then (*) else (+) -- this is where number=Int should be applied!

applyIntBinop (op True) 4 5 -- at this AST node, how do we even know to check for a supertype value?
```

So... when we combine higher-order functions with supertypes, shit gets complicated
We need help from the compiler
At almost any arbitrary AST node, a function's supertype can get specialized by some constraint or other

Compile time:

- Insert some param into every AST node type? Yikes
- Create one more AST node variant for type-specialisation? Ooh! Nice!
  - `SuperTypeResolve SuperType SuperTypeMember Expression`
- In "normal" cases we can just generate monomorphic code
- For generic higher-order cases, fall back to generating code for the type-specialization node

Runtime implementation options:

- `SuperTypeClosure` struct

  - contains pointers to monomorphized Closures (2 or 7 of them)
  - at the type specialization node, select one of the children
  - the only bottom-level comparable function is `compare`... so have 7 of those?

- Clone the Closure and write to some supertype field(s) in it
  - you'd have to know a reliable place to write to
    - needs to be the same for any Closure regardless of partial application
    - could reserve a slot in every Closure for the 3 supertypes
      - 4-bit tag \* 3 = 12 bits (easiest code)
      - Or 1 bit for Number, 1 for Appendable, 3 for Comparable = 5 bits
    - we never really needed `n_args` anyway, it's obvious from size, so those 16 bits are available

## value headers

Maybe the type tag doesn't need as many bits anymore if the supertype info is in a new place
It just needs layout info really. The size and the index of the first pointer.

| Struct          | 1st child | unboxed flag bits |
| --------------- | --------- | ----------------- |
| Float           | 0         | 0                 |
| String          | 0         | 0                 |
| FieldGroup      | 0         | 0                 |
| JsRef           | 0         | 0                 |
| List            | 1         | 1                 |
| Tuple2          | 1         | 2                 |
| Tuple3          | 1         | 3                 |
| Record          | 2         | 32                |
| Custom          | 3         | 32                |
| Closure         | 3         | 32                |
| GcStackEmpty    | 2         | 0                 |
| GcStackPush     | 2         | 0                 |
| GcStackPop      | 2         | 0                 |
| GcStackTailCall | 2         | 0                 |

### 32-bit (Wasm)

| float     | string    | fieldgroup | jsref     | list           | tuple2          | tuple3          | record      | custom    | closure     |
| --------- | --------- | ---------- | --------- | -------------- | --------------- | --------------- | ----------- | --------- | ----------- |
| header(0) | header(0) | header(0)  | header(0) | header(1)/flag | header(1)/flags | header(1)/flags | header(2)   | header(3) | header(4)   |
| pad       | data      | field0     | index     | ^child0        | ^child0         | ^child0         | flags       | flags     | flags       |
| data      | data      | field1     |           | ^child1        | ^child1         | ^child1         | ^fieldgroup | ctor      | super/arity |
| data      | data      | field2     |           |                |                 | ^child2         | ^child0     | ^child0   | eval        |
|           | data      | field3     |           |                |                 |                 | ^child1     | ^child1   | ^child0     |
|           | data      | field4     |           |                |                 |                 | ^child2     | ^child2   | ^child1     |
|           | ...       | ...        |           |                |                 |                 | ...         | ...       | ...         |

### 64-bit

| float     | string    | fieldgroup    | jsref     | list           | tuple2          | tuple3          | record      | custom    | closure     |
| --------- | --------- | ------------- | --------- | -------------- | --------------- | --------------- | ----------- | --------- | ----------- |
| header(0) | header(0) | header(0)     | header(0) | header(1)/flag | header(1)/flags | header(1)/flags | header(1)   | header(2) | header(3)   |
| pad       | data      | field0        | index     | pad            | pad             | pad             | flags       | flags     | flags       |
| --------- | --------- | ------------- | --------- | -------------- | --------------- | --------------- | ----------- | --------- | ----------  |
| data      | data      | field1/field2 |           | ^child0        | ^child0         | ^child0         | ^fieldgroup | ctor      | super/arity |
| data      | data      | field3/field4 |           | ^child1        | ^child1         | ^child1         | ^child0     | ^child0   | eval        |
|           | data      | ...           |           |                |                 | ^child2         | ^child1     | ^child1   | ^child0     |
|           | data      |               |           |                |                 |                 | ^child2     | ^child2   | ^child1     |
|           | ...       |               |           |                |                 |                 | ...         | ...       | ...         |

## Header

```c
typedef union {
  u32 word;
  struct {
    u8  first_child : 3;
    u8  tuple_flags : 3;   // only valid for List & Tuple (unbox flags)
    u32 size        : 26;  // payload size in pointers (26 bits => 256MB)
  } fields;
} Header;

#define HEADER_FLOAT             (Header) {.fields={ .size=16/SIZE_UNIT }}

#define HEADER_STRING(len16)     (Header) {.fields={ .size=(sizeof(Header) + (2 * len16) + SIZE_UNIT - 1) / SIZE_UNIT }}

#define HEADER_FIELDGROUP(n)     (Header) {.fields={ .size=(sizeof(Header) + (4 * n) + SIZE_UNIT - 1) / SIZE_UNIT }}

#define HEADER_JS_REF            (Header) {.fields={ .size=8/SIZE_UNIT }}

#define HEADER_LIST(flags)      (Header) {.fields={ .first_child=1, .tuple_flags=flags, .size=3 }}

#define HEADER_TUPLE2(flags)    (Header) {.fields={ .first_child=1, .tuple_flags=flags, .size=3 }}

#define HEADER_TUPLE3(flags)    (Header) {.fields={ .first_child=1, .tuple_flags=flags, .size=4 }}

#define HEADER_RECORD(n)     (Header) {.fields={ .first_child=8/SIZE_UNIT,     .size=8/SIZE_UNIT + 1 + n }}

#define HEADER_CUSTOM(n)     (Header) {.fields={ .first_child=8/SIZE_UNIT + 1, .size=8/SIZE_UNIT + 1 + n }}

#define HEADER_CLOSURE(n)    (Header) {.fields={ .first_child=8/SIZE_UNIT + 2, .size=8/SIZE_UNIT + 2 + n }}

```

Could specialise the header for each layout and get rid of the tuple_flags in cases where we don't need it
That would increase max string size by 8x, but it's pretty big already
Code complexity for getting bigger strings is doing this for every size check in the GC:

```c
Header h;
u32 size = h & 0xe0000000 // e=1110
  ? h & 0x03ffffff    // 03=00000011
  : h & 0x1fffffff;   // 1f=00011111
```

## SuperType assignment codegen

https://static.aminer.org/pdf/PDF/000/542/781/implementing_type_classes.pdf
See section on Placeholders

### Source code level

```elm
doAdd a b = a + b
sum list = List.foldl doAdd 0 list
numberTotal = sum [1,2,3] -- Um... what? Should be a compile error... or defualt to Int
intTotal = sum [1,2, round 3]
floatTotal = sum [1.0,2.0,3.0]
```

_Where_ in this program do I need to assign the typeclass?
According to the paper above, I want to insert a "placeholder" at each variable with a `number` in its type
I think each instance of `sum` needs to get one.
So when I'm transforming Canonical AST to Optimized AST, I look for annotated nodes that have a `forAll` in their type
and insert an extra AST node above it that says what typeclass it is and what value it is applying to that

Is the placeholder just an Annotation?
Or it could be a special passthrough `let` that we insert like this:
`3.0 + 5.0` => `let addPlaceholder = (+) in addPlaceholder 3.0 5.0`
The type solver will then work out `addPlaceholder` works on `Float` but `+` still works on `number`
We can spot that and insert code to implement it.

But how do we know to do this at all?
Just rely on knowing the Kernel functions?
What about wrappers like `doAdd`? We need to put a placeholder on that too but how do we figure that out?
Do we need to rely on user annotations?
Ah! We can leave out the placeholder if user annotation says "definitely no SuperTypes here"
But insert one if it's unannotated.

https://static.aminer.org/pdf/PDF/000/542/781/implementing_type_classes.pdf

> Placeholders are inserted when the type checker encounters either an overloaded variable, a method, or a letrec bound variable

Questions:

- Where exactly in the Elm AST do we insert placeholders?

  - overloaded variable (I guess this includes `doAdd` from the example above?)
  - method (presumably `Basics.add` is a _method_ of `number`?)
  - letrec variable
    - we don't really distinguish letrec references from any other local var reference in the Elm AST
    - could just add a placeholder for all VarLocals that have annotated SuperType or no annotation, not that big a deal

- What to do about more than one supertype in a function signature?
  - Well if the placeholder is a `let` with an annotation then the info is all there. Compare it to the original var.
  - That's part of the conversion from Canonical to Optimized

## Stuff to do

- When creating Canonical AST, insert the placeholders

---

# Bytecode ideas

What would a bytecode for Elm look like?

- compile to very low level bytecodes and have a VM to interpret them
- GC stack tracing would be a lot easier for one thing. We're using our own stack instead of the native one so it's easy.
- A stack frame would have one slot per arg and one per `let`
- The VM would be a stack machine!! Could we use the Wasm stack itself?? Meta!!

| AST Expr  | bytecode description                           |
| --------- | ---------------------------------------------- |
| Bool      | load global                                    |
| Chr       | load global                                    |
| Str       | load global                                    |
| Int       | load global                                    |
| Float     | load global                                    |
| VarLocal  | load local                                     |
| VarGlobal | load global                                    |
| VarEnum   | load global                                    |
| VarBox    | load global                                    |
| VarCycle  | load global                                    |
| VarDebug  | load global                                    |
| VarKernel | load global                                    |
| List      | alloc, store heap                              |
| Function  | alloc, store const (fn ptr), store (free vars) |
| Call      | load locals, jump                              |
| TailCall  | load locals, jump                              |
| If        | jumpif, jump                                   |
| Let       | store local                                    |
| Destruct  | heap load at offset                            |
| Case      | jumps                                          |
| Accessor  | load global                                    |
| Access    | heap load at offset                            |
| Update    | alloc, load, lookup field, store               |
| Record    | alloc, store ptrs                              |
| Unit      | load global                                    |
| Tuple     | alloc, store                                   |
| Shader    | load global?                                   |

## VM structure

array of global data
array of bytecodes
stack of intermediate vars
locals array
program counter

## Bytecode instruction set

```hs

data ByteCode
  = LoadGlobal Int        -- index into global array
  | LoadConst Int
  | LoadLocal Int         -- index into locals array
  | StoreLocal Int        -- index into locals array
  | Allocate Int          -- number of pointer-sized slots
  | Clone
  | StoreHeapAtOffset Int -- arity 2 (ref, value)
  | LoadHeapAtOffset Int
  | Jump Int              -- target index in bytecode array
  | JumpIf Int            -- target index in bytecode array
  | LookupFieldConst Int  -- field ID
  | LookupField           -- arity 2 (record, field ID)
  | CallKernel Int Int    -- num args, fn index
```

## VM Pros

- stack tracing is now possible and in fact easy
- the VM can have different implementations - Wasm, C, JS
- could have one that's directly in Wasm

## VM Cons

- There's not really any such thing as a function anymore
- All of this might be bad for optimisation, especially compared to browser JITs

## Translating bytecode to Wasm / machine code

- each bytecode written out in target language
- dispatch is done at compile time
- how does the stack work?
  - could make our own global array for it
  - actual C stack would not really be used
- no C functions
  - could have one big switch statement, where each case is an Elm function.
- with C functions

  - better for inlining... although we can't really inline anything anyway since we have indirect calls and can't detect saturated calls.

- custom stack!
  - use a global array
  - whenever you enter a function you push the args to the custom stack, then call a C evaluator function that has no args
  - the evaluator loads the args from the custom stack
  - the C stack is only used to keep track of return addresses
- During GC, there's still no way to clear registers

Let's say we want optimisation within a function only. Can still beat Elm JS with that, because JS version of A2, A3 is unoptimisable

```c
void* stack[10240];
void* stack_idx;

size_t some_elm_func_eval() {
  void* arg0 = stack[--stack_idx];
  void* arg1 = stack[--stack_idx];
  void* arg2 = stack[--stack_idx];
  void* let0;
  void* let1;

  // bytecode
  // bytecode
  // bytecode
}
```

# Notes from talking to Evan

## Json-like library for encoding/decoding Wasm, with code gen from types

Specific use case: generate jsToWasmMsg, knowing Msg
from building blocks
jsToWasmInt: JS function. Takes a number, writes header and value to ArrayBuffer
jsToWasmFloat: JS function. Takes a number, writes header and value to ArrayBuffer
jsToWasmString
jsToWasmRecord
jsToWasmCustom
jsToWasmClosure

OK basically this is the wrapper but with lots of stuff partially-compiled into it
Don't need to detect types, don't need the WasmBuilder stuff. It's more stripped down and faster.

Calling a JS kernel function from Wasm

wasmToJsInt
wasmToJsFloat
wasmToJsString
wasmToJsRecord
wasmToJsCustom
wasmToJsClosure

## type inf

extra param on canonical
every ast node has a type variable
type inference works up from leaf of AST
compiler/src/Type/Solve.hs

## fixing the custom vs tuple issue

in ast/optimized.hs

path data type needs a new constructor, same as index but different for custom vs built-ins
optimized/expression uses them differently
destruct and its helpers
Change Index to a different word, follow the errors, fix them all to the two new ones

## msg constructors

builder/src/Generate.hs
debug loads types

compiler/src/Generate/JavaScript/Expression.hs
toDebugMetadata
generateMain
Opt.Dynamic msgType decoder
Extract.fromMsg

could generate a Json decoder from the Msg type
Port.toEncoder
then decode either int or float

port declarations get added to optimize AST in Module.hs
Optimize.Module.addPort

what does optimize do?

- drop stuff to shrink elmo files, which are Opt AST
- most of the compile time for big proj is reading that file

## Evan's notes

Generate.debug
-- calls Generate.loadTypes which loads in
-- all the interface files (.elmi) that will
-- allow us to get the full Msg type
--
-- that info is carried around in
-- Generate.Mode.Dev (Just interfaces)
Generate.JavaScript.Expression.toDebugMetadata
-- place where you start crawling message type
Elm.Compiler.Type.Extract.fromMsg
-- crawl types to gather complete Msg type
Optimize.Port.toEncoder/toDecoder
-- example of converting types to decoders,
-- used in Optimize.Module.addPort to turn
-- ports into "normal optimized nodes"

## Issues for GitHub

- Optimise saturated calls to some Basics/Utils functions as in JS

  - `<|` and `|>`
  - Basics math ops
    `generateCoreCall`
  - Entire math expressions (make them register only, eliminate heap access)

- Get type type info to code gen
  - Eliminate Int/Float ambiguity from JS kernel to Wasm app (this is actually a hard requirement for correctness. Elm Wasm is broken without it.)
    - Special list of kernel fns that return Floats from the runtime to the app. Use this list in code gen.
  - Type-specialised inlining of Int/Float operations
  - Get rid of dynamic type detection in destructuring `Utils_destruct_index`

# TODO: Code gen bug fixes

- [x] `elm_core_List_foldrHelper`, need to pre-declare the `Closure` in the case of non-tail self-recursion.
- [x] `eval_elm_core_List_foldl` getting generated without any `args` array!

# TODO: Code gen improvements

- [ ] get rid of unreachable `tmpN = NULL;` in TCE
  - Could use some "empty expression" marker like `CommentExpr "unreachable code after TCE goto"`
- [ ] TCE args assignments are backwards (still correct, just unintentional)
- [ ] get rid of extra nested blocks in `if` statements
- [x] fix warning assigning ctor to `void*` tmp var in case destructuring
- [ ] in nested case like `eval_elm_core_Dict_removeHelpPrepEQGT` the generation of nested `if` looks weird. Correct but hard to read.

# Tail Calls

- TailCall expression
  - `generateTailCallArg`
    - generate arg expression
    - create tmp
    - declare tmp initialised to expression
  - fold `generateTailCallArg` over args list
  - insert a list of statements
    - GC
    - assign tmp vars to arg array members
    - goto
- Let TailDef expression
  - v similar to `generateLocalFn`, re-use most of that
  - create tce eval and main eval
    - tce eval has extra arg
    - insert label statement (maybe easiest with null statement)
- global TailDef
  - TCE eval
  - main eval
  - Closure
- local TailDef
  - TCE eval
  - main eval
- Note: It's impossible to have an anonymous function literal that is tail recursive. You need a function name to call in the tail! So it must always be in a let or at top level

# STATUS 20 Dec 2019

- generated app is working!
- build system is really awkward
- Expressions to do
  - [x] Case
  - [x] Let Def TailDef
- Nodes to do
  - [x] Cycle values https://gist.github.com/evancz/07436448b7d6c947f21742dab46d1218
  - [x] PortIncoming
  - [x] PortOutgoing
  - [ ] DefineTailFunc (not used in SPA example)
  - [x] sanitise lambda arg names, can be `$`!

# STATUS 10 Dec 2019

- C generation
  - missing creation and registration of `mainsArray` (currenty reading out garbage)
- JS generation
  - complete! :)
  - But wrong naming style for manager `command`! Should have dollars, whereas everything else has underscores
    - Add the command as a JS thunk?
      - Maybe need to amend SharedDefs with a Bool or an ADT or an extra constructor
      - Or make room for the JS name in the shared defs
      - Or make it a Map instead of a Set where values are code
      - It ends up being pretty awkward to combine these different things. Same but different. Annoying.
    - Or make `_Platform_leaf` the thunk
      - The command is a C global
      - Initialise it by a call, or use a pre-applied Closure
        - If pre-applied we'll get Platform_leaf as an unused Closure
          - Wasted code gen but C compiler will DCE
        - If dynamic call then we get one Closure off heap and one on heap
      - Also need a string literal
        - It's tricky to make an ES.String out of a modulename
        - the types are a pain, need to change phantom type
          - just add a fn to the Utf8 module but it pollutes the safety
          - make a local type changer identity function inside a `let`
        - Maybe use Chars/Builder in the SharedDef (will end up as Chars for escaping)
          - but need to do the conversion every time

# STATUS 7 Dec 2019

- Generated C code is error-free and compiling to Wasm! Woohoo!
- has some warnings about pointer types
- I'm not actually generating code to invoke the wrapper though, so app not working yet.

## TODO

- Generate code to invoke the wrapper

  - Approach

    - Just generate `main` in C with no special case (a suspended JS call)
    - Also generate JS for `main`, which is just reading it from Wasm.
    - Normal JS platform code will do the right thing from there.
    - WasmWrapper lib is not needed.

  - Subtasks
    - get rid of special-case mainRecord stuff
    - refactor the handwritten example?
    - refactor the TS and C for the wrapper
    - create an array of mains just like the array of fieldgroups
      - oh yeah, need to rename that too, on the Haskell side?
    - TS just gives you main

* [x] `generateIf`: blockItems in `else` branches hoisted to the top. Perf issue.

---

# TODO 3 Dec 2019

## errors thrown by C compiler

- [x] universal character stuff is wrong, not allowed to use `\u` format for basic charset
  - can just make up a custom encoding with underscores, using double underscore for underscore itself
- [x] need code gen for custom constructor like `author_project_Main_SetCounter`
  - they are uncurried in JS version I think
  - need the shared enum stuff too
- [x] need to implement free vars for `x_func` inside `eval_elm_core_Task_map_lambda0`
- [x] `macro "NEW_RECORD" passed 7 arguments, but takes just 3` (array literal)
  - Adding parens makes it go away
- missing values

  - [x] elm_core_Task_command
    - See `generateManager` and `generateLeaf`
    - looks like we do need a line or two of C as well as the JS
    ```js
    var $elm$core$Task$command = _Platform_leaf('Task');
    var $elm$core$Task$subscription = _Platform_leaf('Task');
    ```
    - can implement this as a pre-curried Closure and a string literal.
    - check existing code for accessor

- [x] elm_core_Task_Perform
- [x] VirtualDom_node
- [x] elm_virtual_dom_VirtualDom_Normal
- wrong names
  - [x] Utils_equal
  - [x] String_fromNumber

# TODO 1 Dec 2019

- [x] `generateIf`: blockItems in `else` branches will get hoisted to the top, getting eagerly evaluated when they shouldn't be. Need to detect blockItems and create a new block for them.
  - might be a new concept here of resetting the block state or something
- maybe investigate State monad for expression stuff
- generate functions in local scope
- free variables

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
- will also need global pre-decls for lambdas

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
