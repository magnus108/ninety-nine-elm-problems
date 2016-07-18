import Html exposing (text)

main =
    let
        result =
            List.map test tests `andMap` solutions
                |> toString
    in
        text result

andMap : List (a -> b) -> List a -> List b
andMap funcs vals =
    List.concatMap (\f -> List.map f vals) funcs

type ListItem a = Single a | Multiple Int a

encode1 : List a -> List (ListItem a)
encode1 list =
    case list of
        [] -> []
        [ x ] -> [ Single x]
        x :: xs ->
            case encode1 xs of
                [] -> []
                Single y :: ys ->
                    if x == y then
                        Multiple 2 y::ys
                    else
                        Single x :: Single y ::ys
                Multiple n y :: ys ->
                    if y == x then
                        Multiple (n + 1) y :: ys
                    else
                        Single x :: Multiple n y :: ys

solutions : List (List a -> List (ListItem a))
solutions =
    [ encode1
    ]

tests : List (List Int, List (ListItem Int))
tests =
    [ ( [ 1, 1, 1, 2, 2, 4], [Multiple 3 1, Multiple 2 2, Single 4] )
    , ( [1], [Single 1] )
    , ( [], [] )
    ]

test : ( List Int, List (ListItem Int)) ->
        ( List Int -> List (ListItem Int) ) -> Bool
test ( result, expect ) solution =
    (solution result) == expect
