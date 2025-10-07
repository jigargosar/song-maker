module Utils exposing (..)

import Basics.Extra as BE
import Set exposing (Set)



-- BASIC UTILS


format : String -> List ( String, String ) -> String
format templateString replacements =
    List.foldr (\( a, b ) -> String.replace a b) templateString replacements


indices : Int -> List Int
indices n =
    List.range 0 (n - 1)


times fn i =
    indices i |> List.map fn


atLeast : comparable -> comparable -> comparable
atLeast =
    BE.atLeast


atMost : comparable -> comparable -> comparable
atMost =
    BE.atMost


justIf : Bool -> a -> Maybe a
justIf condition value =
    if condition then
        Just value

    else
        Nothing



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
