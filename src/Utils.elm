module Utils exposing (..)

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



-- BASIC VIEW UTILS


px : Float -> String
px f =
    String.fromFloat f ++ "px"
