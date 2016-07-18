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

dropEvery1 : Int -> List a -> List a
dropEvery1 n list =
    let
        indexed = List.indexedMap (,) list
        maybeAdd (i, x) xs =
            if (i + 1) % n == 0 then xs else x :: xs
    in
        List.foldr maybeAdd [] indexed

dropEvery2 : Int -> List a -> List a
dropEvery2 n list =
    fst <| List.foldr (\x (acc, i) ->
        ( if i % n == 0
            then acc
            else x::acc
        , i - 1)) ([], List.length list) list

dropEvery3 : Int -> List a -> List a
dropEvery3 n list =
    let
        drop : Int -> List a -> List a
        drop i xs =
            case (i, xs) of
                (_, []) -> []
                (1, _::xs) -> drop n xs
                (_, x::xs) -> x::drop (i - 1) xs
    in
        drop n list

solutions : List (List a -> List a)
solutions =
    [ dropEvery1 2
    , dropEvery2 2
    , dropEvery3 2
    ]

tests : List (List Int, List Int)
tests =
    [ ( [2, 2, 2, 2, 3, 3, 3, 3], [2, 2, 3, 3] )
    , ( [1], [1] )
    , ( [], [] )
    ]

test : ( List Int, List Int) ->
        ( List Int -> List Int ) -> Bool
test ( result, expect ) solution =
    (solution result) == expect
