module Generate.WebAssembly.Memory where

  import Data.Word (Word8, Word32, Word64)

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
    | Function Arity Env Pointer -- arity env body
    | Call NameDeref Arity [NameDeref] -- func argsLen args
    | TailCall NameDeref Arity [NameDeref] -- func argsLen args
    | If Length8 [(Pointer, Pointer)] Pointer -- numIfElseIfs ifElseIfs finalElse
    | Let Boolean EnvIndex Pointer Pointer -- recursive envIdx letBody inBody
    | Destruct NameDeref Length8 [Word32]  -- root numIndices indices
    --      label root      decider         jumps
    -- | Case N.Name N.Name (Decider Choice) [(Int Expr)]
    -- How to handle Case?
    | Accessor RecordFieldIdx -- fieldNameIdx
    | Access Pointer RecordFieldIdx -- record fieldNameIdx
    | Update Pointer Length32 [(RecordFieldIdx, Pointer)] -- record numFields [()]
    | Record Length32 [(RecordFieldIdx, Pointer)]
    | Unit
    | Tuple Pointer Pointer Pointer
    | Shader

  type Length8 = Word8
  type Length32 = Word32

  type Boolean = Word8
  type Pointer = Word32
  type Arity = Word8
  type RecordFieldIdx = Word32
  type EnvIndex = Word32

  data NameDeref =
    NameDeref Word8 Word8 -- scopes outward, env index

  data Env =
    Env Pointer Length32 [Pointer] -- parent length values



  class MemoryObject a where
    bytes :: a -> Int

  instance MemoryObject Word8 where
    bytes _ = 1

  instance MemoryObject Word32 where
    bytes _ = 4

  instance MemoryObject Word64 where
    bytes _ = 8

  instance MemoryObject NameDeref where
    bytes (NameDeref a b) = bytes a + bytes b

  instance MemoryObject a => MemoryObject [a] where
    bytes [] = 0
    bytes (h : t) = bytes h + bytes t

  instance (MemoryObject a, MemoryObject b) => MemoryObject (a, b) where
    bytes (a , b) = bytes a + bytes b

  instance MemoryObject Env where
    bytes (Env parent length values) =
      bytes parent + bytes length + bytes values

  instance MemoryObject ExprMemory where
    bytes expr =
      1 + case expr of
        Bool x ->
          bytes x
        Chr x ->
          bytes x
        Str len list ->
          bytes len + bytes list
        Int x ->
          bytes x
        Float x ->
          bytes x
        VarLocal x ->
          bytes x
        VarGlobal x ->
          bytes x
        VarEnum enumIdx idx ->
          bytes enumIdx + bytes idx
        VarBox x ->
          bytes x
        VarCycle x ->
          bytes x
        VarDebug ->
          0
        VarKernel x ->
          bytes x
        List h t ->
          bytes h + bytes t
        Function arity env bodyptr ->
          bytes arity + bytes env + bytes bodyptr
        Call fun arity args ->
          bytes fun + bytes arity + bytes args
        TailCall fun arity args ->
          bytes fun + bytes arity + bytes args
        If len ifElseIfs finalElse ->
          bytes len + bytes ifElseIfs + bytes finalElse
        Let recursive envIdx letBody inBody ->
          bytes recursive + bytes envIdx + bytes letBody + bytes inBody
        Destruct name len path ->
          bytes name + bytes len + bytes path
        -- Case
        Accessor field ->
          bytes field
        Access record field ->
          bytes record + bytes field 
        Update record len pairs ->
          bytes record + bytes len + bytes pairs
        Record len pairs ->
          bytes len + bytes pairs
        Unit ->
          0
        Tuple x y z ->
          bytes x + bytes y + bytes z
        Shader ->
          0

  {-
    Decider Choice
      Leaf Choice
      Chain
        { _testChain
        , _success
        , _failure
        }
      Fanout

    Choice
      Inline
      Jump
  -}








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
