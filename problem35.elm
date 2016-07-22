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

factors1 : Int -> List Int
factors1 n =
    let
        aux d n =
            if n == 1 then
                []
            else if n `rem` d == 0 then
                d :: aux d (n // d)
            else
                aux (d + 1) n
    in
        aux 2 n

factors2 : Int -> List Int
factors2 n =
    case n of
        1 -> []
        n ->
            let
                prime = List.head <| dropWhile (rem n >> (/=)0) [2 ..n]
            in
                case prime of
                    Nothing -> []
                    (Just x) ->
                        ((::)x) <| factors2 <| n // x

dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
  case list of
    []      -> []
    x::xs   ->
        if (predicate x) then
            dropWhile predicate xs
        else
            list

solutions : List (Int -> List Int)
solutions =
    [ factors1
    , factors2
    ]

tests : List (Int, List Int, List Int -> List Int -> Bool )
tests =
    [ ( 315, [3, 3, 5, 7], (==)  )
    , ( 2, [2], (==) )
    ]

test : ( Int, List Int, List Int -> List Int -> Bool ) ->
      (Int -> List Int) -> Bool
test ( result, expect, compare ) solution =
    compare (solution result) expect
