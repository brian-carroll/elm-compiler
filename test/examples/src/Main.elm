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
        [ text (String.fromInt (functionLiteral 123 456))
        , text stringLiteral
        , text needToEvaluateOnInit
        ]


functionLiteral : Int -> Int -> Int
functionLiteral a b =
    a + b

stringLiteral : String
stringLiteral = "Hello world"


needToEvaluateOnInit : String
needToEvaluateOnInit = "Hello " ++ "world"

 
main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
