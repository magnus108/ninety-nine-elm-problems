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

pack1 : List a -> List (List a)
pack1 list =
    case list of
        [] -> []
        [ x ] -> [[ x ]]
        x :: xs ->
            case pack1 xs of
                [] -> []
                y :: ys ->
                    if List.member x y then
                        (x :: y) :: ys
                    else
                        [ x ] :: y :: ys

solutions : List (List a -> List (List a))
solutions =
    [ pack1
    ]

tests : List (List number, List (List (number)))
tests =
    [ ( [ 1, 1, 1, 2, 2, 1, 1], [[1, 1, 1], [2, 2], [1, 1]] )
    , ( [1], [[1]] )
    , ( [], [] )
    ]

test : ( List number, List (List number) ) ->
        ( List number -> List (List number )) -> Bool
test ( result, expect ) solution =
    (solution result) == expect
