{-# LANGUAGE OverloadedStrings #-}
module Generate.C.Literals
  ( generate
  , Literals(..)
  , empty
  , insertInt
  , insertFloat
  , insertChr
  , insertStr
  , insertFieldAccess
  , insertFieldAccessor
  , insertFieldGroup
  , insertCtor
  , insertKernelJs
  , insertGlobalJs
  , combineFieldLiterals
  )
  where

import qualified Data.ByteString.Builder as B
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set as Set

import qualified Data.Name as N
import qualified AST.Optimized as Opt
import qualified Elm.Float as EF
import qualified Elm.String as ES

import qualified Generate.C.AST as C
import qualified Generate.C.Kernel as CK
import qualified Generate.C.Name as CN


-- Constants with no dependencies, emitted at top of program
-- Some from source, some generated
data Literals =
  Literals
    { litInt :: Set.Set Int
    , litFloat :: Set.Set EF.Float
    , litChr :: Set.Set ES.String
    , litStr :: Set.Set ES.String
    , litFieldAccess :: Set.Set N.Name
    , litFieldAccessor :: Set.Set N.Name
    , litFieldGroup :: Set.Set [N.Name]
    , litCtor :: Set.Set N.Name
    , litKernelJs :: Set.Set (N.Name, N.Name)
    , litGlobalJs :: Set.Set Opt.Global -- JS kernel with Global name format
    }


empty :: Literals
empty =
  Literals
    { litInt = Set.empty
    , litFloat = Set.empty
    , litChr = Set.empty
    , litStr = Set.empty
    , litFieldAccess = Set.empty
    , litFieldAccessor = Set.empty
    , litFieldGroup = Set.empty
    , litCtor = Set.fromList $ map N.fromChars ["LT", "EQ", "GT"]
    , litKernelJs = Set.empty
    , litGlobalJs = Set.empty
    }


{-
      INSERTION HELPERS
-}

insertInt :: Int -> Literals -> Literals
insertInt value literals =
  literals { litInt = Set.insert value (litInt literals) }


insertFloat :: EF.Float -> Literals -> Literals
insertFloat value literals =
  literals { litFloat = Set.insert value (litFloat literals) }


insertChr :: ES.String -> Literals -> Literals
insertChr value literals =
  literals { litChr = Set.insert value (litChr literals) }


insertStr :: ES.String -> Literals -> Literals
insertStr value literals =
  literals { litStr = Set.insert value (litStr literals) }


insertFieldAccess :: N.Name -> Literals -> Literals
insertFieldAccess value literals =
  literals { litFieldAccess = Set.insert value (litFieldAccess literals) }


insertFieldAccessor :: N.Name -> Literals -> Literals
insertFieldAccessor value literals =
  literals { litFieldAccessor = Set.insert value (litFieldAccessor literals) }


insertFieldGroup :: [N.Name] -> Literals -> Literals
insertFieldGroup value literals =
  literals { litFieldGroup = Set.insert value (litFieldGroup literals) }


insertCtor :: N.Name -> Literals -> Literals
insertCtor value literals =
  literals { litCtor = Set.insert value (litCtor literals) }


insertKernelJs :: (N.Name, N.Name) -> Literals -> Literals
insertKernelJs value literals =
  literals { litKernelJs = Set.insert value (litKernelJs literals) }


insertGlobalJs :: Opt.Global -> Literals -> Literals
insertGlobalJs value literals =
  literals { litGlobalJs = Set.insert value (litGlobalJs literals) }



{-
      CODE GEN
-}

generate :: Literals -> [C.ExternalDeclaration]
generate literals =
  generateFieldGroupArray literals : generateStructs literals ++ generateEnums literals


{-
      FIELD HELPERS
-}

generateFieldGroupArray :: Literals -> C.ExternalDeclaration
generateFieldGroupArray (Literals _ _ _ _ _ _ fieldGroups _ _ _) =
  let
    pointerArray = foldr
      (\fields acc ->
        ([], C.InitExpr $ C.Unary C.AddrOp $ C.Var $ CN.fieldGroup fields)
        : acc
      )
      [([], C.InitExpr $ C.Var CN.nullPtr)]
      fieldGroups
  in
  C.DeclExt $ C.Decl
    [C.TypeSpec $ C.TypeDef CN.FieldGroup]
    (Just $ C.Declr (Just CN.wrapperFieldGroups) [C.PtrDeclr [], C.ArrDeclr [] C.NoArrSize])
    (Just $ C.InitExpr $ C.CompoundLit $ pointerArray)


combineFieldLiterals :: Literals -> Set.Set N.Name
combineFieldLiterals (Literals _ _ _ _ access accessors fieldGroups _ _ _) =
  let
    insertGroup :: Set.Set N.Name -> [N.Name] -> Set.Set N.Name
    insertGroup acc fg =
      foldr Set.insert acc fg
  in
  foldl insertGroup (Set.union access accessors) fieldGroups


{-
      ENUMS
-}

generateEnums :: Literals -> [C.ExternalDeclaration]
generateEnums literals@(Literals _ _ _ _ _ _ _ ctors kernelJs globalJs) =
  generateEnumKernelJs kernelJs globalJs
  ++ generateEnumCtors ctors
  ++ (generateEnumFields $ combineFieldLiterals literals)


generateEnumKernelJs :: Set.Set (N.Name, N.Name) -> Set.Set Opt.Global -> [C.ExternalDeclaration]
generateEnumKernelJs kernelJs globalJs =
  let
    names =
      (map (\(home, name) -> CN.jsKernelEval home name) (Set.toList kernelJs)) ++
      (map (\(Opt.Global home name) -> CN.jsGlobalEval home name) (Set.toList globalJs))
  in
  generateEnum names ++ generateEnumDebugInfo "jsValues" names


generateEnumCtors :: Set.Set N.Name -> [C.ExternalDeclaration]
generateEnumCtors ctors =
  let
    names =
      map CN.ctorId $ Set.toList ctors
  in
  generateEnum names ++ generateEnumDebugInfo "ctors" names


generateEnumFields :: Set.Set N.Name -> [C.ExternalDeclaration]
generateEnumFields fields =
  let
    names =
      map CN.fieldId $ Set.toList fields
  in
  generateEnum names ++ generateEnumDebugInfo "fields" names


generateEnum :: [CN.Name] -> [C.ExternalDeclaration]
generateEnum names =
  case names of
    [] ->
      []
    _ ->
      [C.DeclExt $ C.Decl [C.TypeSpec $ C.Enum names] Nothing Nothing]



{-
      ENUM DEBUG INFO
      Map enum values to strings for debugging
-}

generateEnumDebugInfo :: B.Builder -> [CN.Name] -> [C.ExternalDeclaration]
generateEnumDebugInfo arraySuffix names =
  let
    arrayName :: B.Builder
    arrayName = "Debug_" <> arraySuffix

    strings :: C.InitializerList
    strings =
      map
        (\name ->
          ([], C.InitExpr $ C.Const $ C.StrConst (CN.toBuilder name)))
        names
    
    array :: C.ExternalDeclaration
    array =
      C.DeclExt $ C.Decl
        [C.TypeSpec C.Char]
        (Just $ C.Declr (Just $ CN.fromBuilder arrayName)
          [C.PtrDeclr [], C.ArrDeclr [] C.NoArrSize])
        (Just $ C.InitExpr $ C.CompoundLit strings)

    size :: C.ExternalDeclaration
    size =
      C.DeclExt $ C.Decl
        [C.TypeSpec C.Int]
        (Just $ C.Declr (Just $ CN.fromBuilder (arrayName <> "_size")) [])
        (Just $ C.InitExpr $ C.Const $ C.IntConst $ length names)
  in
  [ size
  , array
  ]



{-
      STRUCTS
-}

generateStructs :: Literals -> [C.ExternalDeclaration]
generateStructs literals =
  (map generateInt $ Set.toList $ litInt literals)
  ++ (map generateFloat $ Set.toList $ litFloat literals)
  ++ (map generateChr $ Set.toList $ litChr literals)
  ++ (map generateStr $ Set.toList $ litStr literals)
  ++ (map generateAccessor $ Set.toList $ litFieldAccessor literals)
  ++ (map generateFieldGroup $ Set.toList $ litFieldGroup literals)
  ++ (concatMap generateKernelJs $ Set.toList $ litKernelJs literals)
  ++ (map generateGlobalJs $ Set.toList $ litGlobalJs literals)


generateInt :: Int -> C.ExternalDeclaration
generateInt value =
  CK.generateStructDef CN.ElmInt (CN.literalInt value)
    [ ("header", CK.generateHeader CK.HEADER_INT)
    , ("value", C.Const $ C.IntConst value)
    ]
    Nothing


generateFloat :: EF.Float -> C.ExternalDeclaration
generateFloat value =
  CK.generateStructDef CN.ElmFloat (CN.literalFloat value)
    [ ("header", CK.generateHeader CK.HEADER_FLOAT)
    , ("value", C.Const $ C.FloatConst value)
    ]
    Nothing


generateChr :: ES.String -> C.ExternalDeclaration
generateChr value =
  CK.generateStructDef CN.ElmChar (CN.literalChr value)
    [("header", CK.generateHeader CK.HEADER_CHAR)]
    (Just ("words16", generateUtf16 value))


generateStr :: ES.String -> C.ExternalDeclaration
generateStr value =
  let words16 = generateUtf16 value
  in
  CK.generateStructDef CN.ElmString16 (CN.literalStr value)
    [("header", CK.generateHeader $ CK.HEADER_STRING (length words16))]
    (Just ("words16", words16))


generateAccessor :: N.Name -> C.ExternalDeclaration
generateAccessor name =
  CK.generateClosure (CN.accessor name)
    (C.Unary C.AddrOp $ C.Var CN.utilsAccessEval)
    2 [C.nameAsVoidPtr $ CN.fieldId name]


generateFieldGroup :: [N.Name] -> C.ExternalDeclaration
generateFieldGroup names =
  CK.generateStructDef CN.FieldGroup (CN.fieldGroup names)
    [ ("header", CK.generateHeader $ CK.HEADER_FIELDGROUP (length names))
    , ("size", C.Const $ C.IntConst $ length names)
    ]
    (Just ("fields", map (C.Var . CN.fieldId) names))


generateKernelJs :: (N.Name, N.Name) -> [C.ExternalDeclaration]
generateKernelJs (home, name) =
  if CK.shouldGenStruct home name then
    [CK.generateClosure (CN.kernelValue home name)
      (C.nameAsVoidPtr $ CN.jsKernelEval home name)
      CK.maxClosureArity
      []
    ]
  else
    []


generateGlobalJs :: Opt.Global -> C.ExternalDeclaration
generateGlobalJs (Opt.Global home name) =
  CK.generateClosure (CN.global home name)
    (C.nameAsVoidPtr $ CN.jsGlobalEval home name)
    CK.maxClosureArity
    []


generateUtf16 :: ES.String -> [C.Expression]
generateUtf16 str =
  map (C.Const . C.IntHexConst) $
    concatMap encodeUtf16 $
    unescape $
    ES.toChars str


-- The compiler keeps backslashes that were in the Elm source,
-- since they're also neeeded in the JS output.
-- But here we are outputting UTF-16 hex codes, not C string literals.
unescape :: [Char] -> [Char]
unescape str =
  case str of
    [] -> str
    [_] -> str
    c1 : c2 : rest ->
      if c1 /= '\\' then
        c1 : unescape (c2 : rest)
      else
        case c2 of
          '\\' -> '\\' : unescape rest
          '\'' -> '\'' : unescape rest
          '"'  -> '"'  : unescape rest
          'n'  -> '\n' : unescape rest
          'r'  -> '\r' : unescape rest
          't'  -> '\t' : unescape rest
          _ -> c1 : unescape (c2 : rest)


encodeUtf16 :: Char -> [Int]
encodeUtf16 chr =
  let
    codepoint = Char.ord chr
    (high, low) = quotRem (codepoint - 0x10000) 0x400
  in
  if codepoint < 0x10000 then
    [codepoint]
  else
    [ high + 0xD800
    , low + 0xDC00
    ]
