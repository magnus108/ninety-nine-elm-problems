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

const : a -> b -> a
const x _ = x

duplicate1 : Int -> List a -> List a
duplicate1 n list =
    case list of
        [] -> []
        x::xs ->
            (List.repeat n x) ++ duplicate1 n xs

duplicate2 : Int -> List a -> List a
duplicate2 n xs =
    case xs of
        [] -> []
        x::xs ->
            List.foldr (const(\y -> x::y)) (duplicate2 n xs) [1..n]

duplicate3 : Int -> List a -> List a
duplicate3 n = List.concatMap (List.repeat n)

solutions : List (List a -> List a)
solutions =
    [ duplicate1 2
    , duplicate2 2
    , duplicate3 2
    ]

tests : List (List Int, List Int)
tests =
    [ ( [1, 2, 2, 3, 3], [1, 1, 2, 2, 2, 2, 3, 3, 3, 3] )
    , ( [1], [1, 1] )
    , ( [], [] )
    ]

test : ( List Int, List Int) ->
        ( List Int -> List Int ) -> Bool
test ( result, expect ) solution =
    (solution result) == expect
