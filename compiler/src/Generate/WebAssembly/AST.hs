module Generate.WebAssembly.Ast where

data TypeIdx = TypeIdx Int
data FuncIdx = FuncIdx Int
data TableIdx = TableIdx Int
data MemIdx = MemIdx -- numeric index must be 0, only one data segment allowed
data GlobalIdx = GlobalIdx Int
data LocalIdx = LocalIdx Int
data LabelIdx = LabelIdx Int
data Var indexSpace
  = IndexVar indexSpace
  | NameVar String

data NumType = IntType | FloatType
data NumWidth = Bits32 | Bits64
data Limits = Limits Int (Maybe Int)
type MemType = Limits
data Mutability = Mutable | Immutable

data ValType = ValType NumType NumWidth
data ResultType = ResultType [ValType]

data FuncType = FuncType [ValType] [ValType]
data TableType = TableType Limits ElemType
data ElemType = AnyFunc
data GlobalType = GlobalType Mutability ValType 

data Func =
  Func
  { name :: String
  , type_ :: FuncType 
  , locals :: [ValType]
  , body :: Expr
  }

data DataSegment =
  DataSegment
  { memIdx :: Var MemIdx
  , dataOffset :: Expr
  , bytes :: [Int]
  }

data Import =
  Import String String ImportExportDesc
        
data Export =
  Export String ImportExportDesc

data ImportExportDesc
  = ImpExpFunc FuncIdx
  | ImpExpTable TableIdx
  | ImpExpMem MemIdx
  | ImpExpGlobal GlobalIdx

type Expr = [Instr]

data Instr
  = Unreachable
  | Nop
  | Br (Var LabelIdx)
  | BrIf (Var LabelIdx)
  | BrTable (Var LabelIdx) [(Var LabelIdx)]
  | Return
  | Call (Var FuncIdx)
  | CallIndirect (Var TypeIdx)
  | Drop
  | Select
  | GetLocal (Var LocalIdx)
  | SetLocal (Var LocalIdx)
  | TeeLocal (Var LocalIdx)
  | GetGlobal (Var GlobalIdx)
  | SetGlobal (Var GlobalIdx)
  | MemorySize
  | MemoryGrow
  | FloatMethod FMethod
  | IntMethod IMethod

data FMethod
  = FLoad NumWidth MemArg
  | FStore NumWidth MemArg
  | FConst NumWidth
  | FUnOp NumWidth FUnOpType
  | FBinOp NumWidth FBinOpType
  | FRelOp NumWidth FRelOpType
  | FDemote
  | FPromote
  | FConvert Sx NumWidth NumWidth
  | FReinterpret NumWidth

data IMethod
  = ILoad NumWidth MemArg
  | ILoad_32_8 Sx MemArg
  | ILoad_32_16 Sx MemArg
  | ILoad_64_8 Sx MemArg
  | ILoad_64_16 Sx MemArg
  | ILoad_64_32 Sx MemArg
  | IStore NumWidth MemArg
  | IStore_32_8 MemArg
  | IStore_32_16 MemArg
  | IStore_64_8 MemArg
  | IStore_64_16 MemArg
  | IStore_64_32 MemArg
  | IUnOp NumWidth IUnOpType
  | IBinOp NumWidth IBinOpType
  | IEqz NumWidth
  | IRelOp NumWidth IRelOpType
  | IWrap
  | IExtend Sx
  | ITrunc Sx NumWidth NumWidth
  | IReinterpret NumWidth

data Sx = Signed | Unsigned
data MemArg = MemArg { memOffset :: Int, align :: Int }

data IUnOpType =
  Clz | Ctz | Popcnt

data IBinOpType =
  Iadd | Isub | Imul | Idiv Sx | Irem Sx | Iand
  | Ior | Ixor | Ishl | Ishr Sx | Irotl | Irotr

data FUnOpType =
  Fabs | Fneg | Fsqrt | Fceil | Ffloor | Ftrunc | Fnearest

data FBinOpType =
  Fadd | Fsub | Fmul | Fdiv | Fmin | Fmax | Fcopysign

data IRelOpType =
  Ieq | Ine | Ilt Sx | Igt Sx | Ile Sx | Ige Sx

data FRelOpType =
  Feq | Fne | Flt | Fgt | Fle | Fge

data Module
  = Module
  { types :: [FuncType]
  , funcs :: [Func]
  , tables :: [TableType]
  , mems :: [MemType]
  , globals :: [GlobalType]
  , elem :: [ElemType]
  , data_ :: [DataSegment]
  , start :: Maybe (Var FuncIdx)
  , imports :: [Import]
  , exports :: [Export]
  }
    