import Html exposing (text)
import Problem54 exposing (..)

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

insert : comparable -> Tree comparable -> Tree comparable
insert value tree =
    case tree of
        Empty -> leaf value
        Branch x l r ->
            case compare value x of
                LT -> Branch x (insert value l) r
                GT -> Branch x l (insert value r)
                EQ -> tree

construct1 : List comparable -> Tree comparable
construct1 list =
    List.foldl insert Empty list

leaf : a -> Tree a
leaf x = Branch x Empty Empty

solutions : List (List comparable -> Tree comparable)
solutions =
    [ construct1
    ]

tests : List ( List Int, Tree comparable,
                Tree comparable -> Tree comparable -> Bool )
tests =
    [ ( [3, 2, 5, 7, 1], Branch 3
        (Branch 2 (Branch 1 Empty Empty) Empty)
        (Branch 5 Empty (Branch 7 Empty Empty))
        , (==) )
    , ( [], Empty, (==) )
    ]

test : ( List comparable, Tree comparable,
                Tree comparable -> Tree comparable -> Bool ) ->
                ( List comparable -> Tree comparable) -> Bool
test ( result, expect, comparer ) solution =
    comparer (solution result) expect
