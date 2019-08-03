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
