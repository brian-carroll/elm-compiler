module Generate.WebAssembly.Memory where

  import Data.Word (Word8, Word32, Word64)

  -- rename this
  -- HeapObject, HeapValue, MemoryStructure, RuntimeObject, RuntimeValue, HeapStruct
  data ExprMemory
    = Bool Boolean
    | Chr Word32
    | Str Length32 [Word32]
    | Int Word32
    | Float Word64
    | VarLocal NameDeref
    | VarGlobal Pointer
    | VarEnum Word32 Word32 -- enumId, index
    | VarBox Pointer -- what's this boxing business?
    | VarCycle Pointer
    | VarDebug
    | VarKernel Pointer
    | List Pointer Pointer -- head tail
    | Function Arity Pointer Pointer -- arity env body
    | Call NameDeref Arity [NameDeref] -- func argsLen args
    | TailCall NameDeref Arity [NameDeref] -- func argsLen args
    | If Pointer Length8 [(Pointer, Pointer)] -- finalElse numIfElseIfs ifElseIfs
    | Let Boolean EnvIndex Pointer Pointer -- recursive envIdx letBody inBody
    | Destruct NameDeref Length8 [Word32]  -- root numIndices indices
    --        label    root      decider         jumps
    -- | Case N.Name N.Name (Decider Choice) [(Int Expr)]
    -- How to handle Case?
    | Accessor RecordFieldIdx -- fieldNameIdx
    | Access Pointer RecordFieldIdx -- record fieldNameIdx
    | Update Pointer Length32 [(RecordFieldIdx, Pointer)] -- record numFields [()]
    | Record Length32 [(RecordFieldIdx, Pointer)]
    | Unit
    | Tuple2 Pointer Pointer
    | Tuple3 Pointer Pointer Pointer
    | Shader
    -- Env
    -- Decider


    {-
      data Decider a
        = Leaf a
        | Chain
            { _testChain :: [(DT.Path, DT.Test)]
            , _success :: Decider a
            , _failure :: Decider a
            }
        | FanOut
            { _path :: DT.Path
            , _tests :: [(DT.Test, Decider a)]
            , _fallback :: Decider a
            }

      data Choice
        = Inline Expr
        | Jump Int
      

      So here's my understanding so far
      - a decider is generated from a Case expression
      - a leaf is an expression you evaluate when you make the decision
      - 'inline' means the expression is generated at the leaf
      - 'jump' means you first jump outwards to reach the expression
          - JS is a labelled "break", Wasm can be similar
      - a path is a sequence of accesses ('stuff.thing' in JS) to get to a value
      - a test is a pattern to match agains the value we found on the path
          - (ctor, cons, nil, tuple   are just structural matching
              int, chr, str, bool   compare against values
          - we don't have ctor's anywhere yet, they are Nodes rather than Exprs
          - in prod mode, 0.19 has ctorToInt (see generateIfTest in Expression.hs)
          - a chain generates a load of 'if's
      - fanouts generate JS case statements
          Wasm br_table

        Case
          label   dunno, probably for jumping out of loops
          root    The root object we're matching against!!! of course!
          decider 
          jumps


      What does this mean for Elm Wasm?
        - paths! how do we do those?
          - a sequence of accesses, simliar to destruct
          - except no need to go outwards through Envs
          - always matching against the root object
                case root of
                  Thing a 1 -> foo
                  Stuff -> bar
          - compile each case expression into a custom eval function
          - pass in a pointer to the root object
          - leaf expressions can refer to outer scopes
          - but we don't have to evaluate the leaf expr,
              just pass it back to 'eval'
          - 'Case' data structure contains a function reference,
              which is the size of a pointer
      
      Case Pointer FunctionId

    -}


  type Length8 = Word8
  type Length32 = Word32

  type Boolean = Word8
  type Arity = Word8
  type EnvIndex = Word32

  type RecordFieldIdx = Word32
  type Pointer = Word32
  type ExprType = Word8


  -- This should probably be its own expression constructor
  -- takes a bit of evaluating
  data NameDeref =
    NameDeref Word8 Word8 -- scopes outward, env index

  data Env =
    Env Pointer Length32 [Pointer] -- parent length values

  data MemoryLayout =
    MemoryLayout
      { program :: ProgramMemory
      , heap :: [ExprMemory]
      }

  data ProgramMemory =
    ProgramMemory
      { null :: Word32
      , gcState :: GcMemory
      , elmGlobals :: [ExprMemory]
      }

  data GcMemory =
    GcMemory
      { nextFree :: Pointer
      , firstLive :: Pointer
      }
