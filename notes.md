- can I get type info into the generator somehow?
- C.generate is called from Generate.c
  - has access to
    - modules :: [Build.Module]
      - Fresh _ I.Interface _
      - Cached \_ \_ (MVar CachedInterface)
        - CachedInterface
          - Unneeded
          - Loaded I.Interface
          - Corrupted
      - Elm.Interface.Interface
        { \_values :: Map.Map Name.Name Can.Annotation }
        - Annotation
          - Forall FreeVars Type

```hs
-- module Elm.Interface
data Interface =
  Interface
    { _home    :: Pkg.Name
    , _values  :: Map.Map Name.Name Can.Annotation
    , _unions  :: Map.Map Name.Name Union
    , _aliases :: Map.Map Name.Name Alias
    , _binops  :: Map.Map Name.Name Binop
    }

-- module AST.Canonical
data Annotation = Forall FreeVars Type

-- module AST.Canonical
data Type
  = TLambda Type Type
  | TVar Name
  | TType ModuleName.Canonical Name [Type]
  | TRecord (Map.Map Name FieldType) (Maybe Name)
  | TUnit
  | TTuple Type Type (Maybe Type)
  | TAlias ModuleName.Canonical Name [(Name, Type)] AliasType
  deriving (Eq)

-- module Elm.Compiler.Type.Extract
newtype Types = Types (Map.Map ModuleName.Canonical Types_)
data Types_ =
  Types_
    { _union_info :: Map.Map Name.Name Can.Union
    , _alias_info :: Map.Map Name.Name Can.Alias
    }
merge :: Types -> Types -> Types -- Map.union type1 type2
mergeMany :: [Types] -> Types -- foldr merge
fromInterface :: ModuleName.Raw -> I.Interface -> Types
fromDependencyInterface :: ModuleName.Canonical -> I.DependencyInterface -> Types


```

- These interfaces are either stored in files or fresh
- Generate.c is already running code to load them

```hs
do objects <- finalizeObjects =<< loadObjects root details modules
```

- Seems to be throwing away some of the data in the above line
- `loadObject` throws away the interface by the looks of it.

- Holy shit, there's a fucking `loadTypes` function
  - uses Elm.Compiler.Type.Extract.fromInterface
  - produces `Extract.Types`, a map from canonical name to unions and aliases, without straight-up annotations
  - drops the `_values` field of Elm.Interface.Interface that has the annotations
- `loadTypes` gets used in generator in debug mode. `Mode.Dev (Just types)`
- Only ever used in one function, `Generate.JavaScript.Expression.toDebugMetadata`

- Maybe I can make a `loadTypesC` that does something similar but keeps the good stuff
