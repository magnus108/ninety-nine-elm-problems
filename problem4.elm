import Html exposing (text)

main =
    let
        result =
            List.map test data
                |> toString
    in
        text result

countElements : List a -> Int
countElements xs =
    case xs of
        [] ->
            0
        x::xs ->
            1 + countElements xs

data : List ( Int, number )
data =
    [ ( countElements [1..100], 100 )
    , ( countElements [1], 1 )
    , ( countElements [], 0 )
    ]

test : ( a, a ) -> Bool
test ( result, expect ) =
    result == expect
