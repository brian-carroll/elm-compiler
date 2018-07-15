{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.WebAssembly.Builder
  ( instrToBuilder
  , toBuilder
  , buildValType
  , Declaration
  )
  where

import Prelude hiding (lines, id)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder (Builder)
import Data.Monoid ((<>))

import Generate.WebAssembly.AST
import Generate.WebAssembly.Instructions (i32_const)

-- HELPERS


deeper :: Builder -> Builder
deeper indent =
  "  " <> indent


concatWith :: Builder -> [Builder] -> Builder
concatWith separator builderList =
  mconcat $ List.intersperse separator builderList


-- DECLARATIONS

class Declaration a where
  toBuilder :: a -> Builder


instance Declaration Function where
  toBuilder (Function functionId params locals resultType body) =
    let
      signature =
        concatWith " " $
          "func"
          : buildFunctionId functionId
          : (map (buildSignature "param") params)
          ++ (map (buildSignature "local") locals)
          ++ (Maybe.maybeToList $
                fmap (\rt -> "result " <> buildValType rt) resultType)

      instrBuilders =
        map (instrToBuilder "  ") body
    in
      "("
        <> (concatWith "\n  " (signature : instrBuilders))
        <> ")"

buildSignature :: Builder -> (LocalId, ValType) -> Builder
buildSignature keyword (localId, valType) =
  "(" <> keyword <> " " <> buildLocalId localId
    <> " " <> buildValType valType <> ")"


instance Declaration Import where
  toBuilder (Import level1Builder level2Builder descriptor) =
    "(import "
      <> "\"" <> level1Builder <> "\" "
      <> "\"" <> level2Builder <> "\" "
      <> buildDescriptor descriptor
      <> ")"


instance Declaration Export where
  toBuilder (Export jsName descriptor) =
    "(export "
      <> "\"" <> jsName <> "\" "
      <> buildDescriptor descriptor
      <> ")"
        

buildDescriptor :: ImportExportDesc -> Builder
buildDescriptor descriptor =
  case descriptor of
    ImpExpFunc functionId -> "(func " <> buildFunctionId functionId <> ")"
    ImpExpTable TableIdxZero -> "(table 0)"
    ImpExpMem MemIdxZero -> "(memory 0)"
    ImpExpGlobal globalId -> "(global " <> buildGlobalId globalId <> ")"


instance Declaration Global where
  toBuilder (Global globalId mut valType valueInstr) =
    let
      valueBuilder =
        instrToBuilder "" valueInstr

      typeBuilder =
        case mut of
          Mutable -> "(mut " <> buildValType valType <> ")"
          Immutable -> buildValType valType
    in
      "(global "
        <> buildGlobalId globalId <> " "
        <> typeBuilder <> " "
        <> valueBuilder <> ")"


instance Declaration Table where
  toBuilder table =
    case table of
      TableDeclaration limits AnyFunc ->
        "(table 0 " <> buildLimits limits <> " anyfunc)"

      TableInlineDef AnyFunc functionIds ->
        "(table anyfunc "
          <> toBuilder (ElementSegment 0 functionIds)
          <> ")"


instance Declaration Memory where
  toBuilder (Memory MemIdxZero limits) =
      "(memory 0 " <> buildLimits limits <> ")"

      
buildLimits :: Limits -> Builder
buildLimits (Limits initSize maybeMaxSize) =
  let
    maxBuilderList =
      Maybe.maybeToList $ fmap B.int32Dec maybeMaxSize
  in
    concatWith " " $
      B.int32Dec initSize : maxBuilderList


instance Declaration DataSegment where
  toBuilder (DataSegment MemIdxZero dataOffset builder) =
    let
      dataOffsetBuilder =
        instrToBuilder "" $ i32_const dataOffset
    in
      "(data "
        <> dataOffsetBuilder
        <> " \""
        <> builder
        <> "\")"


instance Declaration ElementSegment where
  toBuilder (ElementSegment tableOffset functionIds) =
    let
      offsetBuilder =
        instrToBuilder "" (i32_const tableOffset)
    in
      case functionIds of
        [] ->
          ""

        [fid] ->
          "(elem " <> offsetBuilder <> " " <> buildFunctionId fid <> ")"

        fids -> 
          "(elem " <> offsetBuilder <> "\n  "
            <> (concatWith "\n  " $ map buildFunctionId fids)
            <> ")"


instance Declaration StartFunction where
  toBuilder (Start functionId) =
    "(start " <> buildFunctionId functionId <> ")"


-- INSTRUCTIONS

instrToBuilder :: Builder -> Instr -> Builder
instrToBuilder indent instr =
  let
    deeperIndent =
      deeper indent

    newLineDeeper =
      "\n" <> deeperIndent

    concatLines builders =
      concatWith newLineDeeper builders

    concatSpaces builders =
      concatWith " " builders

    addParens body =
      "(" <> body <> ")"
  in
    addParens $
      case instr of
        Unreachable ->
          "unreachable"

        Nop ->
          "nop"

        Block labelId valType instrList ->
            buildBlock "block" deeperIndent labelId valType instrList
            <> indent <> "end"

        Loop labelId valType instrList ->
            buildBlock "loop" deeperIndent labelId valType instrList
            <> indent <> "end"

        IfElse valType cond (mThenLabel, thenExpr) (mElseLabel, elseExpr) ->
          let
            condBuilder =
              instrToBuilder deeperIndent cond

            thenLabelList =
              Maybe.maybeToList $ fmap buildLabelId mThenLabel

            elseLabelList =
              Maybe.maybeToList $ fmap buildLabelId mElseLabel

            thenBuilder =
              concatLines $
                thenLabelList ++ 
                map (instrToBuilder deeperIndent) thenExpr

            elseBuilder =
              concatLines $
                elseLabelList ++ 
                map (instrToBuilder deeperIndent) elseExpr
          in
            "if " <> (buildValType valType) <> "\n"
              <> deeperIndent <> thenBuilder
              <> indent <> "else\n"
              <> deeperIndent <> elseBuilder
              <> indent <> "end\n"
              <> deeperIndent <> condBuilder

        Br labelId ->
          "br " <> (buildLabelId labelId)

        BrIf labelId condInstr ->
          "br_if " <> buildLabelId labelId
                <> newLineDeeper
                <> instrToBuilder deeperIndent condInstr

        BrTable branchLabels defaultLabel condInstr ->
          let          
            branchBuilders =
              map buildLabelId branchLabels

            condBuilder =
              instrToBuilder deeperIndent condInstr
          in
            "br_table\n"
              <> (concatLines branchBuilders)
              <> newLineDeeper <> (buildLabelId defaultLabel)
              <> newLineDeeper <> condBuilder

        Return ->
          "return"

        Call fid args ->
          concatLines $
            ("call " <> buildFunctionId fid)
            : (map (instrToBuilder deeperIndent) args)


        CallIndirect typeId indexInstr args ->
          concatLines $
            ("call_indirect " <> buildTypeId typeId)
            : (map (instrToBuilder deeperIndent) (indexInstr : args))


        GetLocal localId ->
          "get_local " <> buildLocalId localId


        SetLocal localId value ->
          "set_local " <> buildLocalId localId
            <> newLineDeeper <> instrToBuilder deeperIndent value


        TeeLocal localId value ->
          "tee_local " <> buildLocalId localId
            <> newLineDeeper <> instrToBuilder deeperIndent value


        GetGlobal gid ->
          "get_global " <> buildGlobalId gid


        SetGlobal gid value ->
          "set_global " <> buildGlobalId gid
            <> newLineDeeper <> instrToBuilder deeperIndent value


        Drop droppedInstr ->
          "drop " <> newLineDeeper <> instrToBuilder deeperIndent droppedInstr


        Select instr0 instr1 testInstr ->
          concatLines $
            "select"
            : map (instrToBuilder deeperIndent) [instr0, instr1, testInstr]


        ConstOp valBuilder valType ->
          buildValType valType <> ".const " <> valBuilder


        Op opCodeBuilder operands ->
          concatLines $
            opCodeBuilder : (map (instrToBuilder deeperIndent) operands)


        MemOp opCodeBuilder (MemArg offset memAlign) operands ->
          let
            builders =
              map (instrToBuilder deeperIndent) operands

            firstLine = concatSpaces $
              opCodeBuilder
                : ("offset=" <> B.intDec offset)
                : (buildMemAlign memAlign)
          in
            concatLines $ firstLine : builders


buildBlock :: Builder -> Builder -> LabelId -> ValType -> [Instr] -> Builder
buildBlock opcode deeperIndent labelId valType instrList =
  let
    firstLine =
      opcode
        <> " " <> buildValType valType
        <> " " <> buildLabelId labelId
        <> "\n"

    builders =
      map (instrToBuilder deeperIndent) instrList
  in
    firstLine <>
      (concatWith ("\n" <> deeperIndent) builders)


buildValType :: ValType -> Builder
buildValType valType =
  case valType of
    I32 -> "i32"
    I64 -> "i64"
    F32 -> "f32"
    F64 -> "f64"


buildLabelId :: LabelId -> Builder
buildLabelId id =
  case id of
    LabelIdx i -> B.intDec i
    LabelName b -> b


buildLocalId :: LocalId -> Builder
buildLocalId id =
  case id of
    LocalIdx i -> B.intDec i
    LocalName b -> b


buildGlobalId :: GlobalId -> Builder
buildGlobalId id =
  case id of
    GlobalIdx i -> B.intDec i
    GlobalName b -> b


buildFunctionId :: FunctionId -> Builder
buildFunctionId id =
  case id of
    FunctionIdx i -> B.intDec i
    FunctionName b -> b


buildTypeId :: TypeId -> Builder
buildTypeId id =
  case id of
    TypeIdx i -> B.intDec i
    TypeName b -> b


buildMemAlign :: MemAlign -> [Builder]
buildMemAlign align =
  case align of
    Align8 -> ["align=3"]
    Align16 -> ["align=4"]
    Align32 -> ["align=5"]
    Align64 -> ["align=6"]
    AlignNatural -> []
