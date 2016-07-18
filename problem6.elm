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

isPalindrome1 : List a -> Bool
isPalindrome1 xs =
    List.reverse xs == xs

solutions : List ( List a -> Bool )
solutions =
    [ isPalindrome1
    ]

tests : List ( List number, Bool )
tests =
    [ ( [1, 2, 3, 2, 1], True )
    , ( [1, 2, 2, 1], True )
    , ( [1], True )
    , ( [], True )
    ]

test : ( List number, Bool ) -> ( List number -> Bool ) -> Bool
test ( result, expect ) solution =
    (solution result) == expect
