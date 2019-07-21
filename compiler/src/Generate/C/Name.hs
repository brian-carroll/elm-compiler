module Generate.C.Name
where

import qualified Language.C as C
import qualified Data.Name as Name
import qualified Generate.C.Builder as CB


ctorPrefix :: String
ctorPrefix = "Ctor"


fieldPrefix :: String
fieldPrefix = "Field_"


identFromField :: Name.Name -> C.Ident
identFromField field =
  CB.identWithPrefix fieldPrefix field
      

identFromCtor :: Name.Name -> C.Ident
identFromCtor field =
  CB.identWithPrefix ctorPrefix field


idFieldGroup :: C.Ident
idFieldGroup = CB.identFromChars "FieldGroup"

