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

encode1 : List a -> List (number, a)
encode1 list =
    case list of
        [] -> []
        [ x ] -> [ (1, x) ]
        x :: xs ->
            case encode1 xs of
                [] -> []
                (n, y) :: ys ->
                    if x == y then
                        (n+1, x)::ys
                    else
                        (1, x)::(n, y)::ys

dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
    case list of
        [] -> []
        x::xs ->
            if (predicate x) then
                dropWhile predicate xs
            else
                list

takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
    case list of
        [] -> []
        x::xs ->
            if (predicate x) then
                x :: takeWhile predicate xs
            else
                []

encode2 : List a -> List (Int, a)
encode2 list =
    case list of
        [] -> []
        x::xs as y ->
            (List.length (takeWhile ((==)x) y), x) ::
                encode2 (dropWhile ((==)x) xs)

solutions : List (List a -> List (Int, a))
solutions =
    [ encode1
    , encode2
    ]

tests : List (List Int, List (Int, Int))
tests =
    [ ( [ 1, 1, 1, 2, 2, 1, 1], [(3,1),(2,2),(2,1)] )
    , ( [1], [(1,1)] )
    , ( [], [] )
    ]

test : ( List Int, List (Int, Int)) ->
        ( List Int -> List (Int, Int) ) -> Bool
test ( result, expect ) solution =
    (solution result) == expect
