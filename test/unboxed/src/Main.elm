module Main exposing (main)

import Browser
import Html exposing (Html, div, text, br)


-- doAdd : number -> number -> number
doAdd a b =
    a + b     -- Need to unbox or not? And which instruction, i32.add or f64.add?


-- sum : List number -> number
sum list =
    List.foldl doAdd 0 list


-- numberTotal : number
-- should be illegal! Can't generate instructions for `number`, only Int or Float
numberTotal =
    sum [1, 2, 3]


-- supertype specialization: needs to affect code gen somehow!
intList : List Int
intList =
    [1, 2, 3]


-- intTotal : Int
intTotal =
    sum [1, 2, 3] 


-- floatTotal : Float
floatTotal =
    sum [1.1, 2.2, 3.3]


main : Html ()
main =
    div []
        [ text <| "intTotal = " ++ String.fromInt intTotal
        , br [] []
        , text <| "floatTotal = " ++ String.fromFloat floatTotal
        ]
