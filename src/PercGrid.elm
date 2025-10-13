module PercGrid exposing
    ( PercGrid
    , PercPos
    , deleteStep
    , fromPercList
    , initial
    , isActive
    , parse
    , serialize
    , setCell
    , shiftStepRight
    )

import Instruments exposing (PercType)
import Internal.Grid as InternalGrid
import Set exposing (Set)
import Timing exposing (StepIdx)


type alias PercGrid =
    Set ( PercRowIdx, StepIdx )


type alias PercRowIdx =
    Int


type alias PercPos =
    { percType : PercType, stepIdx : StepIdx }



-- Basic Grid Operations


initial : PercGrid
initial =
    Set.empty


isActive : PercPos -> PercGrid -> Bool
isActive position grid =
    InternalGrid.get (positionToTuple position) grid


setCell : PercPos -> Bool -> PercGrid -> PercGrid
setCell position isActive_ grid =
    InternalGrid.set (positionToTuple position) isActive_ grid


positionToTuple : PercPos -> ( PercRowIdx, StepIdx )
positionToTuple { percType, stepIdx } =
    ( Instruments.percRowIdx percType, stepIdx )



-- Grid Transformations


shiftStepRight : Int -> Int -> PercGrid -> PercGrid
shiftStepRight fromStepIdx totalSteps grid =
    InternalGrid.shiftColumnRight fromStepIdx totalSteps grid


deleteStep : Int -> Int -> PercGrid -> PercGrid
deleteStep stepToDelete totalSteps grid =
    InternalGrid.deleteColumn stepToDelete totalSteps grid



-- Grid Conversions


fromPercList : List (List PercType) -> PercGrid
fromPercList stepPercussion =
    stepPercussion
        |> List.indexedMap
            (\stepIdx percTypes ->
                List.map
                    (\percType ->
                        ( Instruments.percRowIdx percType, stepIdx )
                    )
                    percTypes
            )
        |> List.concat
        |> Set.fromList



-- Grid Serialization


{-| Convert PercGrid to comma-separated integers: "0,0,1,2,5,10"
-}
serialize : PercGrid -> String
serialize =
    InternalGrid.serialize


{-| Parse comma-separated integers back to PercGrid: "0,0,1,2,5,10"
-}
parse : String -> PercGrid
parse =
    InternalGrid.parse
