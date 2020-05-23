{-# LANGUAGE OverloadedStrings #-}
module Generate.C.Builder
where

import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import qualified Data.Map as Map
import Data.Maybe (maybe, maybeToList)
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Elm.Float as EF

-- import qualified Data.Utf8 as Utf8
import Generate.C.AST as C
import Generate.C.Name (Name)
import Generate.C.Name as CN


-- UTILS


join :: B.Builder -> [B.Builder] -> B.Builder
join sep builders =
  mconcat $ List.intersperse sep builders


joinMap :: B.Builder -> (a -> B.Builder) -> [a] -> B.Builder
joinMap sep buildItem items =
  mconcat $ List.intersperse sep $ map buildItem items


indent1 :: B.Builder
indent1 = "    "


nIndent1 :: B.Builder
nIndent1 = "\n" <> indent1


-- EXPRESSION


fromExpr :: Expression -> B.Builder
fromExpr expression =
  case expression of
    Comma exprList ->
      joinMap ", " fromExpr exprList

    Assign op lval rval ->
      (fromExpr lval) <> opBuilder <> (fromExpr rval)
      where
        opBuilder =
          case op of
            AssignOp -> " = "
            MulAssOp -> " *= "
            DivAssOp -> " /= "
            RmdAssOp -> " %= "
            AddAssOp -> " += "
            SubAssOp -> " -= "
            ShlAssOp -> " <<= "
            ShrAssOp -> " >>= "
            AndAssOp -> " &= "
            XorAssOp -> " ^= "
            OrAssOp -> " |= "
      
    Cond condition expr1 expr0 ->
      (fromExpr condition)
      <> " ? " <> (fromExpr expr1)
      <> " : " <> (fromExpr expr0)

    Binary op lhs rhs ->
      fromBinop op lhs rhs

    Cast typeNameDecl expr ->
      "(" <> fromDeclaration typeNameDecl <> ")" <> fromExpr expr

    Unary op expr ->
      let e = fromExpr expr in
      case op of
        PreIncOp -> "++" <> e
        PreDecOp -> "--" <> e
        PostIncOp -> e <> "++"
        PostDecOp -> e <> "--"
        AddrOp -> "&" <> e
        DerefOp -> "*" <> e
        PlusOp -> "+" <> e
        MinOp -> "-" <> e
        CompOp -> "~" <> e
        NegOp -> "!" <> e

    SizeofExpr expr ->
      "sizeof(" <> (fromExpr expr) <> ")"

    SizeofType typeNameDecl ->
      "sizeof(" <> (fromDeclaration typeNameDecl) <> ")"

    Index array index ->
      (fromExpr array) <> "[" <> (fromExpr index) <> "]"

    Call funcExpr argExprs ->
      (fromExpr funcExpr)
      <> "("
      <> joinMap ", " fromExpr argExprs
      <> ")"

    MemberDot structure member ->
      (fromExpr structure)
      <> "."
      <> (CN.toBuilder member)

    MemberArrow structure member ->
      (fromExpr structure)
      <> "->"
      <> (CN.toBuilder member)

    Var name -> CN.toBuilder name

    Const constant ->
      case constant of
        IntConst int -> B.intDec int
        IntHexConst int -> "0x" <> (B.wordHex $ fromIntegral int)
        CharConst chr -> "'" <> (B.char7 chr) <> "'"
        FloatConst float -> EF.toBuilder float
        StrConst builder -> "\"" <> builder <> "\""

    CompoundLit initList ->
      fromInitList initList

    StatExpr statement -> "/*StatExpr*/"

    Parens expr -> "(" <> (fromExpr expr) <> ")"

    CommentExpr builder -> "/* " <> builder <> " */" 


fromBinop :: BinaryOp -> Expression -> Expression -> B.Builder
fromBinop op lhs rhs =
  let
    bl = fromExpr lhs
    br = fromExpr rhs
  in
  case op of
    MulOp  -> bl <> " * " <> br
    DivOp  -> bl <> " / " <> br
    RmdOp  -> bl <> " % " <> br
    AddOp  -> bl <> " + " <> br
    SubOp  -> bl <> " - " <> br
    ShlOp  -> bl <> " << " <> br
    ShrOp  -> bl <> " >> " <> br
    LtOp   -> bl <> " < " <> br
    GtOp   -> bl <> " > " <> br
    LeOp   -> bl <> " <= " <> br
    GeOp   -> bl <> " >= " <> br
    EqOp   -> bl <> " == " <> br
    NeqOp  -> bl <> " != " <> br
    AndOp  -> bl <> " & " <> br
    XorOp  -> bl <> " ^ " <> br
    OrOp   -> bl <> " | " <> br
    LandOp -> bl <> " && " <> br
    LorOp  -> bl <> " || " <> br

-- STATEMENT


fromStatement :: B.Builder -> Statement -> B.Builder
fromStatement indent statement =
  let
    nIndent = "\n" <> indent
    deeper = indent <> indent1
    nDeeper = nIndent <> indent1
  in
  case statement of
    Label name statement ->
      (CN.toBuilder name) <> ":"
      <> nIndent <> (fromStatement deeper statement)

    Switch expression statement  ->
      "switch (" <> (fromExpr expression) <> ")" 
      <> fromStatement nDeeper statement

    Cases expressions statement ->
      (joinMap (":" <> nIndent) fromExpr expressions) <> ":"
      <> nDeeper <> fromStatement deeper statement

    Default statement ->
      "default:" <> nDeeper
      <> fromStatement deeper statement

    Expr maybeExpression -> 
      maybe "" fromExpr maybeExpression

    Compound revBlockItems ->
      let
        block =
          List.foldl'
            (\acc item ->
              nDeeper <> (fromBlockItem deeper item) <> ";" <> acc)
            ""
            revBlockItems
      in
      "{" <> block <> nIndent <> "}"

    If condition thenStmt maybeElseStmt ->
      "if (" <> (fromExpr condition) <> ") "
        <> (fromStatement indent thenStmt)
        <> (maybe "" (\elseStmt -> 
              " else " <> (fromStatement indent elseStmt))
            maybeElseStmt)

    While guardExpr statement ->
      "while (" <> (fromExpr guardExpr) <> ")"
      <> fromStatement indent statement

    DoWhile guardExpr statement ->
      "do " <> fromStatement indent statement
      <> " while (" <> (fromExpr guardExpr) <> ")"

    For init guardExpr iterExpr statement ->
      "/* For */"

    Goto name ->
      "goto " <> CN.toBuilder name

    Cont ->
      "continue"

    Break ->
      "break"

    Return maybeExpr ->
      (join " " $
        "return" : (map fromExpr $ maybeToList maybeExpr))

    CommentStatement builder ->
      "// " <> builder

    NullStatement ->
      ""


fromBlockItem :: B.Builder -> CompoundBlockItem -> B.Builder
fromBlockItem indent item =
  case item of
    BlockStmt statement -> fromStatement indent statement
    BlockDecl declaration -> fromDeclaration declaration


fromDeclaration :: Declaration -> B.Builder
fromDeclaration (Decl declSpecs maybeDeclarator maybeInitializer) =
  (join " " (map fromDeclSpec declSpecs))
  <> " "
  <> (join " = " $
      (map fromDeclarator $ maybeToList maybeDeclarator)
       ++ (map fromInitializer $ maybeToList maybeInitializer)
      )


fromInitializer :: Initializer -> B.Builder
fromInitializer init =
  case init of
    InitExpr expr -> fromExpr expr
    InitList initList -> fromInitList initList


fromInitList :: InitializerList -> B.Builder
fromInitList initList =
  mconcat (
    "{ "
    : (map fromInitListItem initList)
    ++ ["}"]
  )


fromInitListItem :: ([PartDesignator], Initializer) -> B.Builder
fromInitListItem (parts, init) =
  case parts of
    [] ->
      (fromInitializer init) <> ", "
    _ ->
      (mconcat $ map fromPartDesignator parts)
      <> " = "
      <> fromInitializer init
      <> ", "


fromPartDesignator :: PartDesignator -> B.Builder
fromPartDesignator part =
  case part of
    ArrDesig expression ->
      "[" <> fromExpr expression <> "]" -- not sure of this, but prob won't use
    MemberDesig builder ->
      "." <> builder
    RangeDesig from to ->
      "[" <> fromExpr from <> "..." <> fromExpr to <> "]" -- not sure of this, but prob won't use


fromDeclSpec :: DeclarationSpecifier -> B.Builder
fromDeclSpec declSpec =
  case declSpec of
    TypeSpec typeSpec -> fromTypeSpecifier typeSpec
    TypeQual typeQual -> fromTypeQualifier typeQual


fromTypeSpecifier :: TypeSpecifier -> B.Builder
fromTypeSpecifier typeSpec =
  case typeSpec of
    Void -> "void"
    Int -> "int"
    SizeT -> "size_t"
    Char -> "char"

    Enum names ->
      "enum {"
        <> (List.foldl'
            (\acc name -> acc <> nIndent1 <> CN.toBuilder name <> ",")
            "" names)
        <> "}"

    TypeDef kernelTypeDef ->
      case kernelTypeDef of
        ElmValue -> "ElmValue"
        ElmInt -> "ElmInt"
        ElmFloat -> "ElmFloat"
        ElmChar -> "ElmChar"
        ElmString -> "ElmString"
        ElmString16 -> "ElmString16"
        Cons -> "Cons"
        Tuple2 -> "Tuple2"
        Tuple3 -> "Tuple3"
        Custom -> "Custom"
        Record -> "Record"
        FieldGroup -> "FieldGroup"
        Closure -> "Closure"
        I32 -> "i32"
        U32 -> "u32"
        F64 -> "f64"


fromTypeQualifier :: TypeQualifier -> B.Builder
fromTypeQualifier typeQual =
  case typeQual of
    ConstQual -> "const"


fromDeclarator :: Declarator -> B.Builder
fromDeclarator (Declr maybeName derivedDeclrs) =
  let nameBuilder = maybe "" CN.toBuilder maybeName
  in
  List.foldl' fromDerivedDeclr nameBuilder derivedDeclrs


fromDerivedDeclr :: B.Builder -> DerivedDeclarator -> B.Builder
fromDerivedDeclr declrBuilder derivedDeclr =
  case derivedDeclr of
    PtrDeclr typeQualifiers ->
      join " " $
        "*" : (map fromTypeQualifier typeQualifiers) ++ [declrBuilder]

    ArrDeclr _ arraySize ->
      declrBuilder
        <> "["
        <> (case arraySize of
              NoArrSize -> ""
              ArrSize expr -> fromExpr expr
            )
        <> "]"

    FunDeclr paramDeclarations ->
      declrBuilder
      <> "("
      <> (join ", " $ map fromDeclaration paramDeclarations)
      <> ")"


fromHeaderFile :: HeaderFile -> B.Builder
fromHeaderFile headerFile =
  case headerFile of
    KernelH -> "kernel.h"


fromExtDecl :: ExternalDeclaration -> B.Builder
fromExtDecl extDecl =
  case extDecl of
    DeclExt decl ->
      (fromDeclaration decl) <> ";\n"

    FDefExt (FunDef declSpecs declarator blockItems) ->
      mconcat $
        (map fromDeclSpec declSpecs)
        ++ [ " "
           , fromDeclarator declarator
           , " "
           , fromStatement "" (Compound blockItems)
           , "\n"
           ]

    DefineExt name expr ->
      "#define " <> CN.toBuilder name <> " " <> fromExpr expr <> "\n"

    IncludeExt headerFile ->
      "#include \"" <> fromHeaderFile headerFile <> "\"\n"

    CommentExt comment ->
      "// " <> comment <> "\n"

    BlankLineExt ->
      "\n"
