{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Generate.WebAssembly.Test (rootMap, graph) where
  import qualified Data.Map as Map
  import qualified Data.Set as Set
  import qualified Data.Text as Text

  import qualified AST.Optimized as Opt
  import qualified AST.Module.Name as ModuleName
  import qualified Elm.Package as Pkg
  import qualified Elm.Name as N
  import qualified Generate.WebAssembly.Kernel.Basics as Basics
  import qualified Generate.WebAssembly.Kernel.GC as GC

  {-
    outerScopeValue =
      1

    closure arg1 arg2 =
      outerScopeValue + arg1 + arg2

    curried = 
      closure 2

    answer =
      curried 3

  -}


  rootMap :: Map.Map ModuleName.Canonical Opt.Main
  rootMap =
    Map.singleton moduleName main


  moduleName :: ModuleName.Canonical
  moduleName =
    ModuleName.Canonical Pkg.dummyName "TestModule"


  main :: Opt.Main
  main =
    Opt.Static


  g :: Text.Text -> Opt.Global
  g name =
    Opt.Global moduleName (N.fromText name)


  l :: Text.Text -> Opt.Expr
  l name =
    Opt.VarLocal (N.fromText name)


  kernelNode :: Set.Set Opt.Global -> Opt.Node
  kernelNode deps =
    Opt.Kernel (Opt.KContent [] deps) Nothing


  basicsGlobal :: Opt.Global
  basicsGlobal =
    Opt.Global ModuleName.basics (N.fromText "")


  basicsNode :: Opt.Node
  basicsNode =
    kernelNode $ Set.singleton GC.global


  addGraphKeyDollars :: Opt.Global
  addGraphKeyDollars =
    Opt.Global ModuleName.basics (N.fromText "add")


  addReferenceDollars :: Opt.Expr
  addReferenceDollars =
    Opt.VarGlobal addGraphKeyDollars


  addReferenceUnderscores :: Opt.Expr
  addReferenceUnderscores =
    Opt.VarKernel N.basics (N.fromText "add")
  
      

  graph :: Map.Map Opt.Global Opt.Node
  graph =
    Map.fromList
      [ ( basicsGlobal
        , basicsNode
        )
      , ( GC.global
        , kernelNode Set.empty
        )
      , ( addGraphKeyDollars
        , Opt.Define addReferenceUnderscores $
            Set.singleton basicsGlobal
        )
      , ( g "outerScopeValue"
        , Opt.Define (Opt.Int 1) Set.empty
        )
      , ( g "closure"
        , Opt.Define
            (Opt.Function
              [N.fromText "a", N.fromText "b"]
              (Opt.Call addReferenceDollars
                [ Opt.VarGlobal (g "outerScopeValue")
                , Opt.Call addReferenceDollars
                    [ l "a"
                    , l "b"
                    ]
                ]
              )
            )
            (Set.fromList
              [ g "outerScopeValue"
              , addGraphKeyDollars
              , basicsGlobal
              , GC.global
              ])
        )
      , ( g "curried"
        , Opt.Define
            (Opt.Call (Opt.VarGlobal (g "closure")) [Opt.Int 2])
            (Set.singleton $ g "closure")
        )
      , ( g "main"
        , Opt.Define
            (Opt.Call (Opt.VarGlobal (g "curried")) [Opt.Int 3])
            (Set.singleton $ g "curried")
        )
      ]
