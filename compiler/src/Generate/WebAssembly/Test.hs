{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Generate.WebAssembly.Test where
  import qualified Data.Map as Map
  import qualified Data.Set as Set
  import qualified Data.Text as Text

  import qualified AST.Optimized as Opt
  import qualified AST.Module.Name as ModuleName
  import qualified Elm.Package as Pkg
  import qualified Elm.Name as N

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

  -- data Global = Global ModuleName.Canonical N.Name


  testModule :: ModuleName.Canonical
  testModule =
    ModuleName.Canonical Pkg.dummyName "testModule"


  g :: Text.Text -> Opt.Global
  g name =
    Opt.Global testModule (N.fromText name)


  l :: Text.Text -> Opt.Expr
  l name =
    Opt.VarLocal (N.fromText name)


  add :: Opt.Expr
  add =
    Opt.VarGlobal (g "add")


  testClosures :: Map.Map Opt.Global Opt.Node
  testClosures =
    Map.fromList
      [ ( g "outerScopeValue"
        , Opt.Define (Opt.Int 1) Set.empty
        )
      , ( g "closure"
        , Opt.Define
            (Opt.Function
              [N.fromText "arg1", N.fromText "arg2"]
              (Opt.Call add
                [ Opt.VarGlobal (g "outerScopeValue")
                , Opt.Call add
                    [ l "arg1"
                    , l "arg2"
                    ]
                ]
              )
            )
            (Set.fromList
              [ g "outerScopeValue"
              , g "add"
              ]
            )
        )
      , ( g "curried"
        , Opt.Define
            (Opt.Call (Opt.VarGlobal (g "closure")) [Opt.Int 2])
            (Set.fromList
              [ g "outerScopeValue"
              , g "closure"
              , g "add"
              ]
            )
        )
      , ( g "answer"
        , Opt.Define
            (Opt.Call (Opt.VarGlobal (g "curried")) [Opt.Int 3])
            (Set.fromList
              [ g "outerScopeValue"
              , g "closure"
              , g "answer"
              , g "add"
              ]
            )
        )
      ]
