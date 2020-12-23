module Placeholders exposing (main)

import Browser
import Html exposing (Html, div, text, br)


-- doAdd : number -> number -> number
doAdd a b =
    let
        -- placeholder trick doesn't work here!
        -- typeclass value needs to get passed down
        placeholder = (+)
    in
    placeholder a b


-- sum : List number -> number
sum list =
    let
        placeholder1 = List.foldl
        placeholder2 = doAdd
    in
    placeholder1 placeholder2 0 list


-- numberTotal : number
-- should be illegal! Can't generate instructions for `number`, only Int or Float
numberTotal =
    let
        placeholder = sum
    in
    placeholder [1, 2, 3]


-- supertype specialization: needs to affect code gen somehow!
intList : List Int
intList =
    [1, 2, 3]


-- intTotal : Int
intTotal =
    let
        placeholder = sum
    in
    placeholder [1, 2, 3] 


-- floatTotal : Float
floatTotal =
    let
        placeholder = sum
    in
    placeholder [1.1, 2.2, 3.3]


main : Html ()
main =
    div []
        [ text <| "intTotal = " ++ String.fromInt intTotal
        , br [] []
        , text <| "floatTotal = " ++ String.fromFloat floatTotal
        ]
