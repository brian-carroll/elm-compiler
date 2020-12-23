module Original exposing (main)

import Browser
import Html exposing (Html, div, text, br)


-- doAdd : number -> number -> number
doAdd a b =
    a + b     -- Need to unbox or not? And which instruction, i32.add or f64.add?


-- sum : List number -> number
sum list =
    List.foldl doAdd 0 list


-- numberTotal : number
-- Can't generate instructions for `number`, only Int or Float
-- A supertype can never escape the program via effects BUT that's not good enough!
-- We cannot even construct this `List number`!
-- The Optimized AST represents these number literals as Ints so that's what we'd do
-- BUT when targeting JS it's OK to have inconsistencies! Not with a typed target.
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
