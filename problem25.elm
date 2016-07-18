import Html exposing (text)
import Random exposing (int)

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

randomSelect1 : List Int -> List Int
randomSelect1 xs =
    randomSelect2 (List.length xs) xs

randomSelect2 : Int -> List a -> List a
randomSelect2 n list =
    let
        rnd : Int -> List a -> List a -> List a
        rnd n acc xs =
            let
                r = Random.int 0 ( ( List.length xs ) - 1)
            in
                case ( n, xs ) of
                    (0, _) -> acc
                    (n, xs) ->
                        rnd (n - 1) ( acc ++ (slice1 1 1 xs)) (removeAt1 1 xs)
    in
        rnd n [] list

slice1 : Int -> Int -> List a -> List a
slice1 j k = List.drop (j - 1) >> List.take (k - j + 1)

removeAt1 : Int -> List a -> List a
removeAt1 n list = List.take (n - 1) list ++ List.drop n list

solutions : List (List Int -> List Int)
solutions =
    [ randomSelect1
    ]

tests : List ( List Int, List Int )
tests =
    [ ( [1, 2], [1, 2] )
    , ( [1], [1] )
    , ( [], [] )
    ]

test : ( List Int, List Int ) ->
    ( List Int -> List Int) -> Bool
test ( result, expect ) solution =
    (solution result) == expect
