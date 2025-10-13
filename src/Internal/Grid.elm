module Internal.Grid exposing
    ( delete
    , fromString
    , isActive
    , setCell
    , shift
    , toString
    )

import Set exposing (Set)
import Utils exposing (..)


{-| Check if a cell is active in the grid
-}
isActive : ( Int, Int ) -> Set ( Int, Int ) -> Bool
isActive cell grid =
    Set.member cell grid


{-| Set or unset a cell in the grid
-}
setCell : ( Int, Int ) -> Bool -> Set ( Int, Int ) -> Set ( Int, Int )
setCell cell isActive_ grid =
    if isActive_ then
        Set.insert cell grid

    else
        Set.remove cell grid


{-| Shift all cells at or after fromStepIdx one step to the right
Cells that would exceed totalSteps are removed
-}
shift : Int -> Int -> Set ( Int, Int ) -> Set ( Int, Int )
shift fromStepIdx totalSteps grid =
    setFilterMap
        (\( rowId, stepIdx ) ->
            if stepIdx >= fromStepIdx then
                let
                    newStepIdx =
                        stepIdx + 1
                in
                if newStepIdx < totalSteps then
                    Just ( rowId, newStepIdx )

                else
                    Nothing

            else
                Just ( rowId, stepIdx )
        )
        grid


{-| Delete a step, shifting all subsequent steps left
-}
delete : Int -> Int -> Set ( Int, Int ) -> Set ( Int, Int )
delete stepToDelete totalSteps grid =
    if stepToDelete < 0 || stepToDelete >= totalSteps then
        grid

    else
        setFilterMap
            (\( rowId, stepIdx ) ->
                if stepIdx == stepToDelete then
                    Nothing

                else if stepIdx > stepToDelete then
                    Just ( rowId, stepIdx - 1 )

                else
                    Just ( rowId, stepIdx )
            )
            grid


{-| Convert grid to comma-separated string: "0,0,1,2,5,10"
-}
toString : Set ( Int, Int ) -> String
toString grid =
    grid
        |> Set.toList
        |> flattenPairs
        |> List.map String.fromInt
        |> String.join ","


{-| Parse comma-separated string to grid: "0,0,1,2,5,10"
-}
fromString : String -> Set ( Int, Int )
fromString str =
    str
        |> String.split ","
        |> List.filterMap String.toInt
        |> toPairs
        |> Set.fromList



-- Internal Utilities


toPairs : List a -> List ( a, a )
toPairs list =
    case list of
        x :: y :: rest ->
            ( x, y ) :: toPairs rest

        _ ->
            []


flattenPairs : List ( a, a ) -> List a
flattenPairs =
    List.concatMap (\( x, y ) -> [ x, y ])
