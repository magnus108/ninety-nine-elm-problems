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

type NestedList a
    = Elem a
    | List ( List (NestedList a) )

flatten1 : NestedList a -> List a
flatten1 xss =
    case xss of
        (Elem x) ->
            [x]
        (List xs) ->
            List.concatMap flatten1 xs

flatten2 : NestedList a -> List a
flatten2 xss =
    let
        loop acc yss =
            case yss of
                (Elem x) ->
                    x::acc
                (List xs) ->
                    List.foldr (\x acc -> loop acc x) acc xs
    in
        loop [] xss

solutions : List ( NestedList a -> List a)
solutions =
    [ flatten1
    , flatten2
    ]

tests : List ( NestedList number, List number )
tests =
    [ ( List [List [ Elem 1, Elem 2], Elem 3], [1, 2, 3] )
    , ( Elem 1, [1] )
    , ( List [], [] )
    ]

test : ( NestedList number, List number ) ->
    ( NestedList number ->  List number ) -> Bool
test ( result, expect ) solution =
    (solution result) == expect
