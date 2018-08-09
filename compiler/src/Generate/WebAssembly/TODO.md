## How do headers & constructors work?

- Header is only ever written by a constructor so they're linked
- Inside a constructor function you can call the header constructor
- Header constructor takes params:
  - body size
  - Layout = Closure | Container | Primitive
  - constructor ID
  - extra param if it's a closure

## Move 'apply' to Closure module

## Generalise Basics.add to Basics.numOp2

## Preventing time travel

- A full closure at 144 contains a reference to an arg at 192

  - We're creating the closure and then populating it
  - `generateCall` needs to be reordered to generate args first, then the call.

- But what went wrong here? How much danger of this happening a lot?

  - If I'd had a native `A2` then I'd have been forced to write the call as nested expressions, which would have been fine.
  - Took imperative approach to building the closure
  - Creating memory objects is a side-effect in this context! (Just like creating files would be in a normal program!)

- Lesson: constructor functions in native code are probably a really good practice! They sometimes feel/look pointless but they encapsulate the side-effect of creating things and force nested expressions.

## GC header

- Total size for Int is 16 bytes! Eek!
- size + first_pointer + ctor + value

- VLQ can cut header down to 3 bytes, but round up to 4

  - => 8 bytes total for Int with VLQ, which is as good as it gets when boxing

- first_pointer is always a really small number and is known in advance
  - 8: Closure `funcIdx, arity`
  - 4: Record Union Tuple Cons `ctor`
  - 0: Nil Int Float Char String Bool Unit
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

- Closure header

  - Contents
    - Body size (i.e. evaluator arity)
    - Layout = Closure | Container | Primitive
    - Arity
    - ElemIdx
  - For small programs (<255 closures), this is 3 bytes!
  - For big ones (<16384 closures), it's 4 bytes. Basically everything.
  - **Can actually store pointers in ascending order**
    - We now have the total arity in the header
    - To calculate where to put the new args, just subtract current arity from total arity
    - This can save space - only store the args we actually have now,
      without any null pointers
    - BUT then GC size is no longer correct
    - **not actually practical**

- Container header

  - layout (2 bits)
  - dead/alive (1 bit)
  - ctor (1-3 bytes)
  - size (1-3 bytes)

- Primitive header

  - layout (2 bits)
  - dead/alive (1 bit)
  - ctor (1 byte)
  - size (1-3 bytes)
    - string, 3 byte size => 4 + 7 + 7 = 18 bits = 256kB

- Dead/Alive GC bit
  - We need one!! I forgot.
  - This is an independent bit
  - Means that 'body size' shrinks by 1 bit
  - Small values
    - dead/alive = 1 bit
    - layout = 2 bits
    - LEB encoding = 1 bit
    - size = 4 bits
    - this is still fine for most things
  - Extra processing
    - Put the dead/alive in the 6th bit of the LSB of the header
    - Then it just needs to be masked, not right-shifted
    - Should be detectable by just reading once. (i.e. first 32 bits) and applying a mask. The more predictable, the better.
    - Penalty: When decoding size, first byte is different
      - But it was already different! Lower bits.
    - Maybe the processing is easier with a bitshift. Treat the whole header as LEB, then apply some extra masks and downshift. Maybe that's better than having a missing bit in the middle of it.
    - Separates the variable-length encoding from the extra data hidden in the LSB
    - Reduces range of `size` by 1 bit down to 31 bits (2GB string)

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
  - exec
  - constructor

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
