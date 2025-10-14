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
set cell bool grid =
    setUpdate bool cell grid


{-| Shift all cells at or after fromStepIdx one step to the right
Cells that would exceed totalSteps are removed
-}
shiftColumnRight : Int -> Int -> Grid -> Grid
shiftColumnRight fromCol totalCols grid =
    setFilterMap
        (\( rowId, colIdx ) ->
            if colIdx < fromCol then
                Just ( rowId, colIdx )

            else if colIdx + 1 < totalCols then
                Just ( rowId, colIdx + 1 )

            else
                Nothing
        )
        grid


{-| Delete a step, shifting all subsequent steps left
-}
deleteColumn : Int -> Int -> Grid -> Grid
deleteColumn colIdxToDelete totalColumns grid =
    if colIdxToDelete < 0 || colIdxToDelete >= totalColumns then
        grid

    else
        setFilterMap
            (\( rowId, colIdx ) ->
                if colIdx == colIdxToDelete then
                    Nothing

                else if colIdx > colIdxToDelete then
                    Just ( rowId, colIdx - 1 )

                else
                    Just ( rowId, colIdx )
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
