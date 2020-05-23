module Generate.C.Kernel
( shouldGenJsCode
, shouldGenJsEnumId
, shouldGenStruct
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
  | home == Name.fromChars "Json" = (name == Name.fromChars "run")
  | home == Name.utils  = False
  | home == Name.basics = False
  | home == Name.list   = False
  | home == Name.string = False
  | home == Name.char   = False
  | otherwise = True


shouldGenStruct :: Name.Name -> Name.Name -> Bool
shouldGenStruct home name
  | home == Name.fromChars "Json" = False
  | home == Name.utils  = False
  | home == Name.basics = False
  | home == Name.list   = False
  | home == Name.string = False
  | home == Name.char   = False
  | otherwise = True
