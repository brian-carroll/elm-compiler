module Generate.C.Kernel
( shouldGenJsCode
, shouldGenJsEnumId
, shouldGenStruct
, jsonDecodeRunners
)
where

import qualified Data.Name as Name
import qualified Data.Set as Set
import qualified Elm.ModuleName as ModuleName


shouldGenJsCode :: ModuleName.Canonical -> Bool
shouldGenJsCode home
  | home == ModuleName.basics = False
  | home == ModuleName.list   = False
  | home == ModuleName.string = False
  | home == ModuleName.char   = False
  | otherwise = True


shouldGenJsEnumId :: Name.Name -> Name.Name -> Bool
shouldGenJsEnumId home name
  | home == Name.utils  = False
  | home == Name.basics = False
  | home == Name.list   = False
  | home == Name.string = False
  | home == Name.char   = False
  | otherwise = True


shouldGenStruct :: Name.Name -> Name.Name -> Bool
shouldGenStruct home name
  | home == Name.fromChars "Json" = shouldGenStructJson name
  | home == Name.utils  = False
  | home == Name.basics = False
  | home == Name.list   = False
  | home == Name.string = False
  | home == Name.char   = False
  | otherwise = True


shouldGenStructJson :: Name.Name -> Bool
shouldGenStructJson name =
  not (
    (Set.member name jsonDecodeCtors)
    || (Set.member name jsonDecodeRunners)
  )


jsonDecodeCtors :: Set.Set Name.Name
jsonDecodeCtors =
  Set.fromList $ map Name.fromChars $
    [ "succeed"
    , "fail"
    , "decodeInt"
    , "decodeBool"
    , "decodeFloat"
    , "decodeValue"
    , "decodeString"
    , "decodeList"
    , "decodeArray"
    , "decodeNull"
    , "decodeField"
    , "decodeIndex"
    , "decodeKeyValuePairs"
    , "andThen"
    , "oneOf"
    , "map1"
    , "map2"
    , "map3"
    , "map4"
    , "map5"
    , "map6"
    , "map7"
    , "map8"
    , "wrap"
    ]


jsonDecodeRunners :: Set.Set Name.Name
jsonDecodeRunners =
  Set.fromList $ map Name.fromChars $
    [ "run"
    , "runOnString"
    ]
