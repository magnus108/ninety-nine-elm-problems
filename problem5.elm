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

reverse1 : List a -> List a
reverse1 xs =
    let
        rev: List a -> List a -> List a
        rev acc xs =
            case xs of
                [] ->
                    acc
                x::xs ->
                    rev (x::acc) xs
        in
            rev [] xs

reverse2 : List a -> List a
reverse2 xs =
    List.foldl (::) [] xs

reverse3 : List a -> List a
reverse3 xs =
    List.reverse xs

solutions : List ( List a -> List a )
solutions =
    [ reverse1
    , reverse2
    , reverse3
    ]

tests : List ( List number, List number )
tests =
    [ ( [1..4], [4,3,2,1] )
    , ( [1], [1] )
    , ( [], [] )
    ]

test : ( List number, List number ) -> ( List number ->  List number ) -> Bool
test ( result, expect ) solution =
    (solution result) == expect
