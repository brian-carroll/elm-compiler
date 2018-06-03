module NotFound exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)


main : Program () () ()
main =
    Browser.embed
        { init = \_ -> ( (), Cmd.none )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        , view = \_ -> page
        }


page : Html ()
page =
    --   { title = "Page not found"
    --   , body =
    div [] <|
        [ div [ class "not-found" ]
            [ div [ style "font-size" "12em" ] [ text "404" ]
            , div [ style "font-size" "3em" ] [ text "Page not found" ]
            ]
        ]



--   }
