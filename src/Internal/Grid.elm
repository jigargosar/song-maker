module Internal.Grid exposing
    ( deleteColumn
    , get
    , parse
    , serialize
    , set
    , shiftColumnRight
    )

import Set exposing (Set)
import Utils exposing (..)


type alias Grid =
    Set Cell


type alias Cell =
    ( Int, Int )


{-| Check if a cell is active in the grid
-}
get : Cell -> Grid -> Bool
get cell grid =
    Set.member cell grid


{-| Set or unset a cell in the grid
-}
set : Cell -> Bool -> Grid -> Grid
set cell isActive_ grid =
    if isActive_ then
        Set.insert cell grid

    else
        Set.remove cell grid


{-| Shift all cells at or after fromStepIdx one step to the right
Cells that would exceed totalSteps are removed
-}
shiftColumnRight : Int -> Int -> Grid -> Grid
shiftColumnRight fromStepIdx totalSteps grid =
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
deleteColumn : Int -> Int -> Grid -> Grid
deleteColumn stepToDelete totalSteps grid =
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
serialize : Grid -> String
serialize grid =
    grid
        |> Set.toList
        |> flattenPairs
        |> List.map String.fromInt
        |> String.join ","


{-| Parse comma-separated string to grid: "0,0,1,2,5,10"
-}
parse : String -> Grid
parse str =
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
