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

duplicate1 : List a -> List a
duplicate1 list =
    case list of
        [] -> []
        x::xs ->
            x::x::duplicate1 xs

duplicate2 : List a -> List a
duplicate2 = List.foldr (\x acc -> x::x::acc) []

duplicate3 : List a -> List a
duplicate3 = List.concatMap (List.repeat 2)

solutions : List (List a -> List a)
solutions =
    [ duplicate1
    , duplicate2
    , duplicate3
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
