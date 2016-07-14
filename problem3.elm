import Html exposing (text)

main =
    let
        result =
            List.map test data
                |> toString
    in
        text result

elementAt : Int -> List a -> Maybe a
elementAt n xs =
    case xs of
        [] ->
            Nothing
        x::xs ->
            if n == 0
                then Just x
                else elementAt (n - 1) xs

data : List ( Maybe number, Maybe number )
data =
    [ ( elementAt 2 [1..4], Just 3 )
    , ( elementAt 2 [1], Nothing )
    , ( elementAt 2 [], Nothing )
    ]

test : ( a, a ) -> Bool
test ( result, expect ) =
    result == expect
