## Debug

- 1+2+3=1, apparently
- Time travel bugger
  - Trying to apply `add` function to something that doesn't exist yet
  - A full closure at 144 contains a reference to an arg at 192
  - The value at address 192 makes no sense, what is it?

## Test page

- no need for 2 columns, just highlight what was initialised
  - no need for red diff
- Find a way to mark the beginning and end of each _value_
  - maybe alternate striped colours
  - maybe border thickness/colour

## Working prototype

- Add GC header to all values!
- import GC into Basics, Test, etc.
- always add GC to output Wasm
  - Insert into input Graph?
  - Doesn't matter if Kernel stuff doesn't declare dependencies on GC, since order doesn't matter

* GC header

  - Total size for Int is 16 bytes! Eek!
  - size + first_pointer + ctor + value
  - VLQ can cut header down to 3 bytes, but round up to 4
    - => 8 bytes total for Int with VLQ, which is as good as it gets when boxing
  - first_pointer is always a really small number and is known in advance
    - Closure: 8 (ctor + arity)
    - Record: 4 (ctor)
    - Union: 4 (ctor)
    - Tuple: 4 (ctor)
    - Cons: 4 (ctor)
    - Nil: 0
    - Int: 0
    - Float: 0
    - Char: 0
    - String: 0
    - Bool: 0
    - Unit: 0
    - => 2 bits are enough!
  - `size` is always a multiple of 4, could downshift it
    - Containers need multiples of 4 for pointer alignment
    - Int, float, bool, unit, nil => all just ctor
    - Char: 1-4
    - String: any number of bytes
    - For generally making memory line up nicely, each individual value
      should be a multiple of 4. Add padding to Char and String bodies.
  - GC header can use bottom 2 bits for (first_pointer_jump/4) ! :)
    - => Most common header size will be 1 byte, not 2
    - Everything has a ctor, and it's normally small too
    - => can happily squash GC header + ctor into 4 bytes, no hassle
    - When does `size` burst out of its 1-byte range?
      - 7 bits => size 128 => 32 integers. That's big for a Union or Record.
      - 6 bits => size 64 => 16 integers. Still fairly big.

## Refactoring

### State monad for table & data offsets?

- main advantage is using `do` notation to chain generators
  every generator function implicitly gets the state

- State monad has value and state. Value can be the current instruction or list of instructions or whatever.

- could carry around the whole module

  - um, but that doesn't have tablesize or dataoffset as numbers!
  - well unless those go into the Limits
  - 'Memory' limits in AST could be in bytes, Builder converts to pages!
  - or they go into the elements themselves (start and end)
    then search for most recent element to get current position
  - Or just make the Monad State be (Int, Int, Module)

- just solve the problem we have: table and data offset!
  (Int, Int)

- what nice code can I write with this?
  - When I'm inserting 4 declarations, I can write them as `do`
    or chain them with `>>=` or `>>`
  - Are there any places where I'm chaining a few insertions?
    - Dependencies in WebAssembly.hs

### AST FuncType and Global should be in Sets rather than a List?

- Should I do it?

  - It would break the nice WAT source ordering we have.
    (at least for globals)
    functypes are OK to go in a Set, but so far I only have two types
    and only one of them needs to be declared

- How?
  - Both based on Builder, which has no Ord instance
  - Maybe should use Text or something, then convert at the end?
    - Involves refactoring Identifier.hs
    - Need to copy/paste a load of the JS functions rather than
      calling them and converting to Builder.

### Closure Kernel module

- Wasm functions

  - apply
  - call
  - closure constructor
  - destructure/execute

- shared code, more like A2, F2 from Elm JS

  - lots of AN/FN functions or just one of each?
  - Has to be lots
    - For just one, need a sequence of instructions to pile up args on the stack, but that's not allowed.

- A2(closure, a, b)

  - original_arity = bodySize(closure) - 4
  - if original_arity == 2

    - return call_indirect(closure.func, a, b)

  - create new closure
  - insert a and b
  - update remaining arity

  - if new remaining arity /= 0
    - return new closure
  - else
    - return exec(closure)

- exec(closure)

  - loop
    - get size from GC header
    - calculate original arity
    - branch table
      - 0
        - unreachable
      - 1
        - call_indirect
          $i32_i32
          closure.func
          (i32.load offset=8 (get_local 0))
      - 2
        - call_indirect
          $i32i32_i32
          closure.func
          (i32.load offset=12 (get_local 0))
          (i32.load offset=8 (get_local 0))
      - 3
        - call_indirect
          $i32_i32_i32\_\_i32
          closure.func
          (i32.load offset=16 (get_local 0))
          (i32.load offset=12 (get_local 0))
          (i32.load offset=8 (get_local 0))
    - if returned thing is a full closure
      set_local $closure (returned value)
      loop back to top
    - else
      return (returned value)

- Could build the branch table based on the Set of func types built up during code gen. If any arities don't exist, use 'unreachable'
- Or use Haskell laziness?
- Or just pick a big-ish number like in JS?

#### Is it really good to destructure the closure all in one go?

- With real GC, addresses can move any time we allocate, so we'd have to fetch it again and again anyway. Need an indexing function provided by GC.
- It's OK to store pointers in local vars as long as I use GC-provided functions to indirectly access them.
- So it's _not bad_ but is it good?
  - It's one less dereference...
    um... is it? With the virtual pointers? yeah, we have virtual pointers either way so yeah
  - Readability: pretty epic win for readability, to be fair.
    And therefore debuggability
  - Program size - destructuring code shared across all functions

### Memory Manager API

- allocate(n)
- sizeof(structure)
- shallowCopy(structure)
- getCtor(structure)
- index(structure, n)
  - get the nth child

closure stuff

- something to
  - get size (without first pointer thing)
  - get ctor
  - get first word after header
- isClosure(structure)
  - For equality check exception & general tail call elimination

### Closure API

- A1, A2, A3, A4...
  - generate as needed?
  - state at top level? Map or Set
  - or use Haskell laziness??!! Oh that's cool. Infinite Map of A functions.
- (exec is internal only, I think.)
- generateDataSegment(elem_index, arity)
  - sizeof(closure)

* Random thought

  - can I make closure constructors negative?
  - VLQ is still possible, just get 6 bits in first byte rather than 7
  - Should be smaller marker than inserting bit 31
  - clashes with Dict stuff! :(
    - agh! why?
    - `RBNode_elm_builtin` and `RBEmpty_elm_builtin`
    - can I find any code that relies on this?
      - the only thing is `Utils.eqHelp`
      - it tells `==` to call `Dict.toList` before continuing
      - I think Red-Black tree structure depends on order of insertion
    - Doing equality on functions throws an exception
      - Um... how do I throw this exception? I need to do that!
      - JS uses `typeof x === 'function'`
      - This is an actual problem that needs Evan's input

* Closure header
  size + first_pointer + ctor + arity + ptr\*
  In theory I could squish arity in with the header
  But then things are less general. Everything has a ctor, but not everything has an arity

  Actually, I can do the exact same squashing by just limiting everything to 256
  size 256? OK for everything except strings!! Agh!
  first_pointer: OK
  ctor: Pretty much OK
