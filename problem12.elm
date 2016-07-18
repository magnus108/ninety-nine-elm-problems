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

decode1 : List (ListItem a) -> List a
decode1 xs =
    let
        expand x =
            case x of
                Single x -> [x]
                Multiple n x -> List.repeat n x
    in
        List.concatMap expand xs

solutions : List (List (ListItem a) -> List a)
solutions =
    [ decode1
    ]

tests : List (List (ListItem Int), List Int)
tests =
    [ ( [Multiple 3 1, Multiple 2 2, Single 4], [1, 1, 1, 2, 2, 4] )
    , ( [Single 1], [1] )
    , ( [], [] )
    ]

test : ( List (ListItem Int), List Int) ->
        ( List (ListItem Int) -> List Int ) -> Bool
test ( result, expect ) solution =
    (solution result) == expect
