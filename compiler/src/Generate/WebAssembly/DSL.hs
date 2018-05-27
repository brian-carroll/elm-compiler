module Generate.WebAssembly.DSL where

  import Generate.WebAssembly.Ast

  {-
    Domain Specific Language for generating WebAssembly Text format

    Takes advantage of the fact that Wasm text format supports *nested* expressions,
    instead of stack order. This makes composition a lot easier.

    Composition of instructions is type checked
    However semantics are not checked, so it is possible to generate bad Wasm programs
    using this DSL. (Accessing memory address that doesn't exist, etc.)

    Reference:
      https://webassembly.github.io/spec/core/appendix/index-instructions.html
  -}

  unreachable = Unreachable
  nop = Nop
  block = Block
  loop = Loop
  ifElse = IfElse
  br = Br
  br_if = BrIf
  br_table = BrTable
  return_ = Return

  drop = Drop
  select = Select

  get_local = GetLocal
  set_local = SetLocal
  tee_local = TeeLocal
  get_global = GetGlobal
  set_global = GetGlobal

  load :: ValType -> String -> Int -> Instr -> Instr
  load t opCode offset subExpr =
    MemOp
      { opCode = opCode
      , operandTypes = [I32, t]
      , returnType = Nothing
      , subExprs = [subExpr]
      , memarg = MemArg { memOffset = offset, align = Natural }
      }

  i32_load = load I32 "i32.load"
  i64_load = load I64 "i64.load"
  f32_load = load F32 "f32.load"
  f64_load = load F64 "f64.load"
  i32_load8_s = load I32 "i32.load8_s"
  i32_load8_u = load I32 "i32.load8_u"
  i32_load16_s = load I32 "i32.load16_s"
  i32_load16_u = load I32 "i32.load16_u"
  i64_load8_s = load I64 "i64.load8_s"
  i64_load8_u = load I64 "i64.load8_u"
  i64_load16_s = load I64 "i64.load16_s"
  i64_load16_u = load I64 "i64.load16_u"
  i64_load32_s = load I64 "i64.load32_s"
  i64_load32_u = load I64 "i64.load32_u"

  store :: ValType -> String -> Int -> Instr -> Instr -> Instr
  store t opCode offset addrExpr valExpr =
    MemOp
      { opCode = opCode
      , operandTypes = [I32, t]
      , returnType = Nothing
      , subExprs = [addrExpr, valExpr]
      , memarg = MemArg { memOffset = offset, align = Natural }
      }

  i32_store = store I32 "i32.store"
  i64_store = store I64 "i64.store"
  f32_store = store F32 "f32.store"
  f64_store = store F64 "f64.store"
  i32_store8 = store I32 "i32.store8"
  i32_store16 = store I32 "i32.store16"
  i64_store8 = store I64 "i64.store8"
  i64_store16 = store I64 "i64.store16"
  i64_store32 = store I64 "i64.store32"

  memory_size :: Instr
  memory_size =
    Op
      { opCode = "memory.size"
      , operandTypes = []
      , returnType = Just I32
      , subExprs = []
      }

  memory_grow :: Instr -> Instr
  memory_grow subExpr =
    Op
      { opCode = "memory.grow"
      , operandTypes = [I32]
      , returnType = Just I32
      , subExprs = [subExpr]
      }
    
  i32_const :: Int -> Instr
  i32_const x =
    ConstOp
      { literal = show x
      , constType = I32
      }

  i64_const :: Integer -> Instr
  i64_const x =
    ConstOp
      { literal = show x
      , constType = I64
      }

  f32_const :: Float -> Instr
  f32_const x =
    ConstOp
      { literal = show x
      , constType = F32
      }

  f64_const :: Double -> Instr
  f64_const x =
    ConstOp
      { literal = show x
      , constType = F64
      }

  unop :: ValType -> ValType -> String -> Instr -> Instr
  unop ti to opCode subExpr =
    Op
      { opCode = opCode
      , operandTypes = [ti]
      , returnType = Just to
      , subExprs = [subExpr]
      }

  binop :: ValType -> ValType -> String -> Instr -> Instr -> Instr
  binop ti to opCode subExpr1 subExpr2 =
    Op
      { opCode = opCode
      , operandTypes = [ti, ti]
      , returnType = Just to
      , subExprs = [subExpr1, subExpr2]
      }

  i32_eqz = unop I32 I32 "i32.eqz"
  i32_eq = binop I32 I32 "i32.eq"
  i32_ne = binop I32 I32 "i32.ne"
  i32_lt_s = binop I32 I32 "i32.lt_s"
  i32_lt_u = binop I32 I32 "i32.lt_u"
  i32_gt_s = binop I32 I32 "i32.gt_s"
  i32_gt_u = binop I32 I32 "i32.gt_u"
  i32_le_s = binop I32 I32 "i32.le_s"
  i32_le_u = binop I32 I32 "i32.le_u"
  i32_ge_s = binop I32 I32 "i32.ge_s"
  i32_ge_u = binop I32 I32 "i32.ge_u"
  
  i64_eqz = unop I64 I32 "i64.eqz"
  i64_eq = binop I64 I32 "i64.eq"
  i64_ne = binop I64 I32 "i64.ne"
  i64_lt_s = binop I64 I32 "i64.lt_s"
  i64_lt_u = binop I64 I32 "i64.lt_u"
  i64_gt_s = binop I64 I32 "i64.gt_s"
  i64_gt_u = binop I64 I32 "i64.gt_u"
  i64_le_s = binop I64 I32 "i64.le_s"
  i64_le_u = binop I64 I32 "i64.le_u"
  i64_ge_s = binop I64 I32 "i64.ge_s"
  i64_ge_u = binop I64 I32 "i64.ge_u"
  
  f32_eq = binop F32 I32 "f32.eq"
  f32_ne = binop F32 I32 "f32.ne"
  f32_lt = binop F32 I32 "f32.lt"
  f32_gt = binop F32 I32 "f32.gt"
  f32_le = binop F32 I32 "f32.le"
  f32_ge = binop F32 I32 "f32.ge"
  
  f64_eq = binop F64 I32 "f64.eq"
  f64_ne = binop F64 I32 "f64.ne"
  f64_lt = binop F64 I32 "f64.lt"
  f64_gt = binop F64 I32 "f64.gt"
  f64_le = binop F64 I32 "f64.le"
  f64_ge = binop F64 I32 "f64.ge"

  i32_clz = unop I32 I32 "i32.clz"
  i32_ctz = unop I32 I32 "i32.ctz"
  i32_popcnt = unop I32 I32 "i32.popcnt"
  i32_add = binop I32 I32 "i32.add"
  i32_sub = binop I32 I32 "i32.sub"
  i32_mul = binop I32 I32 "i32.mul"
  i32_div_s = binop I32 I32 "i32.div_s"
  i32_div_u = binop I32 I32 "i32.div_u"
  i32_rem_s = binop I32 I32 "i32.rem_s"
  i32_rem_u = binop I32 I32 "i32.rem_u"
  i32_and = binop I32 I32 "i32.and"
  i32_or = binop I32 I32 "i32.or"
  i32_xor = binop I32 I32 "i32.xor"
  i32_shl = binop I32 I32 "i32.shl"
  i32_shr_s = binop I32 I32 "i32.shr_s"
  i32_shr_u = binop I32 I32 "i32.shr_u"
  i32_rotl = binop I32 I32 "i32.rotl"
  i32_rotr = binop I32 I32 "i32.rotr"

  i64_clz = unop I64 I64 "i64.clz"
  i64_ctz = unop I64 I64 "i64.ctz"
  i64_popcnt = unop I64 I64 "i64.popcnt"
  i64_add = binop I64 I64 "i64.add"
  i64_sub = binop I64 I64 "i64.sub"
  i64_mul = binop I64 I64 "i64.mul"
  i64_div_s = binop I64 I64 "i64.div_s"
  i64_div_u = binop I64 I64 "i64.div_u"
  i64_rem_s = binop I64 I64 "i64.rem_s"
  i64_rem_u = binop I64 I64 "i64.rem_u"
  i64_and = binop I64 I64 "i64.and"
  i64_or = binop I64 I64 "i64.or"
  i64_xor = binop I64 I64 "i64.xor"
  i64_shl = binop I64 I64 "i64.shl"
  i64_shr_s = binop I64 I64 "i64.shr_s"
  i64_shr_u = binop I64 I64 "i64.shr_u"
  i64_rotl = binop I64 I64 "i64.rotl"
  i64_rotr = binop I64 I64 "i64.rotr"

  f32_abs = unop F32 F32 "f32.abs"
  f32_neg = unop F32 F32 "f32.neg"
  f32_ceil = unop F32 F32 "f32.ceil"
  f32_floor = unop F32 F32 "f32.floor"
  f32_trunc = unop F32 F32 "f32.trunc"
  f32_nearest = unop F32 F32 "f32.nearest"
  f32_sqrt = unop F32 F32 "f32.sqrt"
  f32_add = binop F32 F32 "f32.add"
  f32_sub = binop F32 F32 "f32.sub"
  f32_mul = binop F32 F32 "f32.mul"
  f32_div = binop F32 F32 "f32.div"
  f32_min = binop F32 F32 "f32.min"
  f32_max = binop F32 F32 "f32.max"
  f32_copysign = binop F32 F32 "f32.copysign"

  f64_abs = unop F64 F64 "f64.abs"
  f64_neg = unop F64 F64 "f64.neg"
  f64_ceil = unop F64 F64 "f64.ceil"
  f64_floor = unop F64 F64 "f64.floor"
  f64_trunc = unop F64 F64 "f64.trunc"
  f64_nearest = unop F64 F64 "f64.nearest"
  f64_sqrt = unop F64 F64 "f64.sqrt"
  f64_add = binop F64 F64 "f64.add"
  f64_sub = binop F64 F64 "f64.sub"
  f64_mul = binop F64 F64 "f64.mul"
  f64_div = binop F64 F64 "f64.div"
  f64_min = binop F64 F64 "f64.min"
  f64_max = binop F64 F64 "f64.max"
  f64_copysign = binop F64 F64 "f64.copysign"

  i32_wrap_i64 = unop I64 I32 "i32.wrap/i64"
  i32_trunc_s_f32 = unop F32 I32 "i32.trunc_s/f32"
  i32_trunc_u_f32 = unop F32 I32 "i32.trunc_u/f32"
  i32_trunc_s_f64 = unop F64 I32 "i32.trunc_s/f64"
  i32_trunc_u_f64 = unop F64 I32 "i32.trunc_u/f64"
  i64_extend_s_i32 = unop I32 I64 "i64.extend_s/i32"
  i64_extend_u_i32 = unop I32 I64 "i64.extend_u/i32"
  i64_trunc_s_f32 = unop F32 I64 "i64.trunc_s/f32"
  i64_trunc_u_f32 = unop F32 I64 "i64.trunc_u/f32"
  i64_trunc_s_f64 = unop F64 I64 "i64.trunc_s/f64"
  i64_trunc_u_f64 = unop F64 I64 "i64.trunc_u/f64"
  f32_convert_s_i32 = unop I32 F32 "f32.convert_s/i32"
  f32_convert_u_i32 = unop I32 F32 "f32.convert_u/i32"
  f32_convert_s_i64 = unop I64 F32 "f32.convert_s/i64"
  f32_convert_u_i64 = unop I64 F32 "f32.convert_u/i64"
  f32_demote_f64 = unop F64 F32 "f32.demote/f64"
  f64_convert_s_i32 = unop I32 F64 "f64.convert_s/i32"
  f64_convert_u_i32 = unop I32 F64 "f64.convert_u/i32"
  f64_convert_s_i64 = unop I64 F64 "f64.convert_s/i64"
  f64_convert_u_i64 = unop I64 F64 "f64.convert_u/i64"
  f64_promote_f32 = unop F32 F64 "f64.promote/f32"
  i32_reinterpret_f32 = unop F32 I32 "i32.reinterpret/f32"
  i64_reinterpret_f64 = unop F64 I64 "i64.reinterpret/f64"
  f32_reinterpret_i32 = unop I32 F32 "f32.reinterpret/i32"
  f64_reinterpret_i64 = unop I64 F64 "f64.reinterpret/i64"
