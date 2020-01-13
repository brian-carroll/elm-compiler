module Main exposing (main)

import Browser
import Html exposing (Html, div, text)


type alias Model =
    ()


initialModel : Model
initialModel =
    ()


type alias Msg =
    ()


update : Msg -> Model -> Model
update () () =
    ()


view : Model -> Html Msg
view () =
    div []
        [ div [] [ text <| String.fromInt (tailCallWrapperFunc 0) ]
        , div [] [ text <| String.fromInt (topLevelTailFunc 0) ]
        ]


tailCallWrapperFunc : Int -> Int
tailCallWrapperFunc x =
    let
        tailFunc xx =
            if xx > 10 then
                xx
            else
                tailFunc (xx + 1)
    in
        tailFunc x


topLevelTailFunc : Int -> Int
topLevelTailFunc xx =
    if xx > 10 then
        xx
    else
        topLevelTailFunc (xx + 1)
        
 
main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
