{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.WebAssembly.Builder (instrToBuilder, toBuilder) where

import Prelude hiding (lines, id)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder (Builder)
import Data.Monoid ((<>))
import Generate.WebAssembly.AST
import Generate.WebAssembly.Instructions (i32_const)

-- HELPERS


data Lines = One | Many deriving (Eq)


linesMap :: (a -> (Lines, b)) -> [a] -> (Bool, [b])
linesMap func xs =
  let
    pairs =
      map func xs
  in
    ( any ((==) Many . fst) pairs
    , map snd pairs
    )


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

      (_, instrBuilders) =
        linesMap (instrToBuilder "  ") body
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
  toBuilder (Global globalId (GlobalType mut valType) valueInstr) =
    let
      (_, valueBuilder) =
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
        "(table 0 " <> toBuilder limits <> " anyfunc)"

      TableInlineDef AnyFunc functionIds ->
        "(table anyfunc "
          <> toBuilder (ElementSegment 0 functionIds)
          <> ")"


instance Declaration Memory where
  toBuilder (Memory MemIdxZero limits) =
      "(memory 0 " <> toBuilder limits <> ")"

      
instance Declaration Limits where
  toBuilder (Limits initSize maybeMaxSize) =
    let
      maxBuilderList =
        Maybe.maybeToList $ fmap B.intDec maybeMaxSize
    in
      concatWith " " $
        B.intDec initSize : maxBuilderList


instance Declaration DataSegment where
  toBuilder (DataSegment MemIdxZero dataOffset bytes) =
    let
      (_, dataOffsetBuilder) =
        instrToBuilder "" $ i32_const dataOffset
    in
      "(data "
        <> dataOffsetBuilder
        <> " \""
        <> B.byteString bytes
        <> "\")"


instance Declaration ElementSegment where
  toBuilder (ElementSegment tableOffset functionIds) =
    let
      (_, offsetBuilder) =
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

instrToBuilder :: Builder -> Instr -> (Lines, Builder)
instrToBuilder indent instr =
  (lines, "(" <> instrBody <> ")")
  where
    deeperIndent =
      deeper indent

    newLineDeeper =
      "\n" <> deeperIndent

    concatLines builders =
      concatWith newLineDeeper builders

    concatSpaces builders =
      concatWith " " builders

    (lines, instrBody) =
      case instr of
        Unreachable ->
          (One, "unreachable")

        Nop ->
          (One, "nop")

        Block labelId valType instrList ->
          (,) Many $
            buildBlock "block" deeperIndent labelId valType instrList
            <> indent <> "end"

        Loop labelId valType instrList ->
          (,) Many $
            buildBlock "loop" deeperIndent labelId valType instrList
            <> indent <> "end"

        IfElse valType cond (mThenLabel, thenExpr) (mElseLabel, elseExpr) ->
          let
            (_, condBuilder) =
              instrToBuilder deeperIndent cond

            thenLabelList =
              Maybe.maybeToList $ fmap buildLabelId mThenLabel

            elseLabelList =
              Maybe.maybeToList $ fmap buildLabelId mElseLabel

            thenBuilder =
              concatLines $
                thenLabelList ++ 
                map (snd . instrToBuilder deeperIndent) thenExpr

            elseBuilder =
              concatLines $
                elseLabelList ++ 
                map (snd . instrToBuilder deeperIndent) elseExpr
          in
            (,) Many $
              "if " <> (buildValType valType) <> "\n"
                <> deeperIndent <> thenBuilder
                <> indent <> "else\n"
                <> deeperIndent <> elseBuilder
                <> indent <> "end\n"
                <> deeperIndent <> condBuilder

        Br labelId ->
          (One, "br " <> (buildLabelId labelId))

        BrIf labelId condInstr ->
          let
            (oneOrMany, builder) =
              instrToBuilder deeperIndent condInstr

            sep =
              case oneOrMany of
                One -> " "
                Many -> newLineDeeper
          in
            ( oneOrMany
            , "br_if " <> buildLabelId labelId
                  <> sep <> builder
            )

        BrTable branchLabels defaultLabel condInstr ->
          let          
            branchBuilders =
              map buildLabelId branchLabels

            (_, condBuilder) =
              instrToBuilder deeperIndent condInstr
          in
            (,) Many $
              "br_table\n"
                <> (concatLines branchBuilders)
                <> newLineDeeper <> (buildLabelId defaultLabel)
                <> newLineDeeper <> condBuilder

        Return ->
          (One, "return")

        Call fid args ->
          let
            (anyMany, argBuilders) =
              linesMap (instrToBuilder deeperIndent) args

            firstLine =
              "call " <> buildFunctionId fid
          in
            if anyMany then
              (Many, concatLines $ firstLine : argBuilders)
            else
              (One, concatSpaces $ firstLine : argBuilders)


        CallIndirect fid args ->
          let
            (anyMany, argBuilders) =
              linesMap (instrToBuilder deeperIndent) args

            firstLine =
              "call_indirect " <> buildFunctionId fid
          in
            if anyMany then
              (Many, concatLines $ firstLine : argBuilders)
            else
              (One, concatSpaces $ firstLine : argBuilders)


        GetLocal localId ->
          (One, "get_local " <> buildLocalId localId)


        SetLocal localId value ->
          case instrToBuilder deeperIndent value of
            (One, builder) ->
              (One, "set_local " <> buildLocalId localId <> " " <> builder)
            (Many, builder) ->
              (Many, "set_local " <> buildLocalId localId
                <> newLineDeeper <> builder)


        TeeLocal localId value ->
          case instrToBuilder deeperIndent value of
            (One, builder) ->
              (One, "tee_local " <> buildLocalId localId <> " " <> builder)
            (Many, builder) ->
              (Many, "tee_local " <> buildLocalId localId
                <> newLineDeeper <> builder)


        GetGlobal gid ->
          (One, "get_global " <> buildGlobalId gid)


        SetGlobal gid value ->
          case instrToBuilder deeperIndent value of
            (One, builder) ->
              (One, "set_global " <> buildGlobalId gid <> " " <> builder)
            (Many, builder) ->
              (Many, "set_global " <> buildGlobalId gid
                <> newLineDeeper <> builder)


        Drop droppedInstr ->
          case instrToBuilder deeperIndent droppedInstr of
            (One, builder) ->
              (One, "drop " <> builder)
            (Many, builder) ->
              (Many, "drop " <> newLineDeeper <> builder)


        Select instr0 instr1 testInstr ->
          let
            (anyMany, builders) =
              linesMap
                (instrToBuilder deeperIndent)
                [instr0, instr1, testInstr]
          in
            if anyMany then
              (Many, concatLines $ "select" : builders)
            else
              (One, concatSpaces $ "select" : builders)


        ConstOp valBuilder valType ->
          (,) One $
            buildValType valType <> ".const " <> valBuilder


        Op opCodeBuilder operands ->
          let
            (anyMany, builders) =
              linesMap (instrToBuilder deeperIndent) operands
          in
            if anyMany then
              (Many, concatLines $ opCodeBuilder : builders)
            else
              (One, concatSpaces $ opCodeBuilder : builders)


        MemOp opCodeBuilder (MemArg offset memAlign) operands ->
          let
            (anyMany, builders) =
              linesMap (instrToBuilder deeperIndent) operands

            firstLine = concatSpaces $
              opCodeBuilder
                : ("offset=" <> B.intDec offset)
                : (buildMemAlign memAlign)
          in
            if anyMany then
              (Many, concatLines $ firstLine : builders)
            else
              (One, concatSpaces $ firstLine : builders)


buildBlock :: Builder -> Builder -> LabelId -> ValType -> [Instr] -> Builder
buildBlock opcode deeperIndent labelId valType instrList =
  let
    firstLine =
      opcode
        <> " " <> buildValType valType
        <> " " <> buildLabelId labelId
        <> "\n"

    builders =
      map (snd . instrToBuilder deeperIndent) instrList
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


buildMemAlign :: MemAlign -> [Builder]
buildMemAlign align =
  case align of
    Align8 -> ["align=3"]
    Align16 -> ["align=4"]
    Align32 -> ["align=5"]
    Align64 -> ["align=6"]
    AlignNatural -> []
