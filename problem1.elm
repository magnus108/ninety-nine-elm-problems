import Html exposing (text)
import Maybe

main =
    let
        result =
            List.map test data
                |> toString
    in
        text result

last : List a -> Maybe a
last xs =
    case xs of
        [] ->
            Nothing
        [x] ->
            Just x
        x::xs ->
            last xs

data : List ( Maybe number, Maybe number )
data =
    [ ( last [1..2], Just 2 )
    , ( last [1], Just 1 )
    , ( last [], Nothing )
    ]

test : ( a, a ) -> Bool
test ( result, expect ) =
    result == expect
