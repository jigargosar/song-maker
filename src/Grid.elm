module Grid exposing
    ( PercGrid
    , PercPos
    , PitchGrid
    , PitchPos
    , convertMelodyToGrid
    , convertPercussionToGrid
    , isPercCellActive
    , isPitchCellActive
    , percPositionToTuple
    , toPercRowIdx
    , updatePercCell
    , updatePitchCell
    )

import Instrument exposing (PercType)
import Scale exposing (RootNote, ScaleType)
import Set exposing (Set)



-- TYPES


type alias PitchPos =
    { pitchIdx : Int, stepIdx : Int }


type alias PitchGrid =
    Set ( Int, Int )


type alias PercPos =
    { percType : PercType, stepIdx : Int }


type alias PercGrid =
    Set ( Int, Int )



-- PITCH GRID HELPERS


isPitchCellActive : PitchPos -> PitchGrid -> Bool
isPitchCellActive { pitchIdx, stepIdx } pitchGrid =
    Set.member ( pitchIdx, stepIdx ) pitchGrid


updatePitchCell : PitchPos -> Bool -> PitchGrid -> PitchGrid
updatePitchCell { pitchIdx, stepIdx } isActive pitchGrid =
    if isActive then
        Set.insert ( pitchIdx, stepIdx ) pitchGrid

    else
        Set.remove ( pitchIdx, stepIdx ) pitchGrid



-- PERCUSSION GRID HELPERS


toPercRowIdx : PercType -> Int
toPercRowIdx percType =
    case percType of
        Instrument.Snare ->
            0

        Instrument.Kick ->
            1


percPositionToTuple : PercPos -> ( Int, Int )
percPositionToTuple { percType, stepIdx } =
    ( toPercRowIdx percType, stepIdx )


isPercCellActive : PercPos -> PercGrid -> Bool
isPercCellActive position grid =
    Set.member (percPositionToTuple position) grid


updatePercCell : PercPos -> Bool -> PercGrid -> PercGrid
updatePercCell position isActive grid =
    let
        tuple =
            percPositionToTuple position
    in
    if isActive then
        Set.insert tuple grid

    else
        Set.remove tuple grid



-- CONVERSION HELPERS


convertMelodyToGrid : List (List String) -> ScaleType -> RootNote -> { start : Int, count : Int } -> Set ( Int, Int )
convertMelodyToGrid stepMelodies scaleType rootNote octaveRange =
    stepMelodies
        |> List.indexedMap
            (\stepIdx noteNames ->
                List.filterMap
                    (\noteName ->
                        let
                            pitchIdx =
                                Scale.noteNameToPitchIdx noteName scaleType rootNote octaveRange
                        in
                        if pitchIdx >= 0 then
                            Just ( pitchIdx, stepIdx )

                        else
                            Nothing
                    )
                    noteNames
            )
        |> List.concat
        |> Set.fromList


convertPercussionToGrid : List (List PercType) -> Set ( Int, Int )
convertPercussionToGrid stepPercussion =
    stepPercussion
        |> List.indexedMap
            (\stepIdx percTypes ->
                List.map
                    (\percType ->
                        ( toPercRowIdx percType, stepIdx )
                    )
                    percTypes
            )
        |> List.concat
        |> Set.fromList
