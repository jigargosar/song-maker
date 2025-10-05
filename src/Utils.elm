module Utils exposing (..)

-- BASIC UTILS

import Set exposing (Set)


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
