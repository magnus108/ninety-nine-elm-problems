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

compress1 : List a -> List a
compress1 xs =
    let
        compress : a -> List a -> List a
        compress x acc =
            case List.head acc of
                Just y ->
                    if x == y then
                        acc
                    else
                        x :: acc
                Nothing ->
                    [x]
    in
        List.foldr compress [] xs

compress2 : List a -> List a
compress2 xs =
    case xs of
        [] -> []
        [x] -> [x]
        x :: y :: xs ->
            if x == y then
                compress2 (x :: xs)
            else
                x :: compress2 (y :: xs)

compress3 : List a -> List a
compress3 list =
    case list of
        [] -> []
        x :: xs ->
            x :: ( compress3 ( dropWhile ((==) x) xs ))


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

span : (a -> Bool) -> List a -> (List a, List a)
span p xs = (takeWhile p xs, dropWhile p xs)

group : List a -> List (List a)
group = groupWhile (==)

groupWhile : (a -> a -> Bool) -> List a -> List (List a)
groupWhile eq xs =
    case xs of
        [] -> []
        x::xs ->
            let
                (ys,zs) = span (eq x) xs
            in
                (x::ys)::groupWhile eq zs

compress4 : List a -> List a
compress4 = List.concatMap (List.take 1) << group

solutions : List (List a -> List a)
solutions =
    [ compress1
    , compress2
    , compress3
    , compress4
    ]

tests : List (List number, List number)
tests =
    [ ( [ 1, 1, 1, 2, 2, 1, 1], [1, 2, 1] )
    , ( [1], [1] )
    , ( [], [] )
    ]

test : ( List number, List number ) -> ( List number -> List number ) -> Bool
test ( result, expect ) solution =
    (solution result) == expect
