module Utils exposing (..)

import Set exposing (Set)



-- BASIC UTILS


format : String -> List ( String, String ) -> String
format templateString replacements =
    List.foldr (\( a, b ) -> String.replace a b) templateString replacements


times fn i =
    List.range 0 (i - 1) |> List.map fn


atLeast : comparable -> comparable -> comparable
atLeast =
    max


atMost : comparable -> comparable -> comparable
atMost =
    min


indices : Int -> List Int
indices n =
    List.range 0 (n - 1)


findFirst : (a -> Bool) -> List a -> Maybe a
findFirst predicate list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just x

            else
                findFirst predicate xs



-- Set Helpers


setUpdate : Bool -> comparable -> Set comparable -> Set comparable
setUpdate b =
    if b then
        Set.insert

    else
        Set.remove


setFilterMap : (a -> Maybe comparable) -> Set a -> Set comparable
setFilterMap fn set =
    set
        |> Set.toList
        |> List.filterMap fn
        |> Set.fromList



-- BASIC VIEW UTILS


px : Float -> String
px f =
    String.fromFloat f ++ "px"
