module Main exposing (..)

import Browser
import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick)
import Random


-- PROBLEM

rollIf : Bool -> Random.Generator Int
rollIf b =
  if b then
    Random.int 1 6
  else
    rollAgain

rollAgain : Random.Generator Int
rollAgain =
  roll

roll : Random.Generator Int
roll =
  Random.map (\n -> n == 1) (Random.int 0 1)
    |> Random.andThen rollIf


-- SUPPORT CODE

main =
  Browser.element
    { init = \() -> (0, Cmd.none)
    , update = update
    , view = view
    , subscriptions = always Sub.none
    }

view model =
  div []
    [ text (String.fromInt model)
    , button [onClick Generate] [text "Generate!"]
    ]

type Msg = Generate | NewValue Int

update msg model =
  case msg of
    Generate ->
      (model, Random.generate NewValue roll)

    NewValue v ->
      (v, Cmd.none)
