module Grid exposing
    ( PitchPos
    , PitchGrid
    , PercPos
    , PercGrid
    , TimeConfig
    , getTotalSteps
    , isPitchCellActive
    , updatePitchCell
    , isPercCellActive
    , updatePercCell
    , noteDuration
    , resizePitchGrid
    , transposePitchGrid
    , convertMelodyToGrid
    , convertPercussionToGrid
    )

import Instruments exposing (PercType)
import Scales exposing (RootNote, ScaleConfig, ScaleType)
import Set exposing (Set)
import Utils exposing (times)


-- Config Types


type alias TimeConfig =
    { bars : Int
    , beatsPerBar : Int
    , subdivisions : Int
    , bpm : Int
    }


-- Grid Types


type alias PitchPos =
    { pitchIdx : Int, stepIdx : Int }


type alias PitchGrid =
    Set ( Int, Int )


type alias PercPos =
    { percType : PercType, stepIdx : Int }


type alias PercGrid =
    Set ( Int, Int )


-- Constants


midiC4 : Int
midiC4 =
    60


-- Basic Grid Operations


empty : Set a
empty =
    Set.empty


isPitchCellActive : PitchPos -> PitchGrid -> Bool
isPitchCellActive { pitchIdx, stepIdx } pitchGrid =
    Set.member ( pitchIdx, stepIdx ) pitchGrid


updatePitchCell : PitchPos -> Bool -> PitchGrid -> PitchGrid
updatePitchCell { pitchIdx, stepIdx } isActive pitchGrid =
    if isActive then
        Set.insert ( pitchIdx, stepIdx ) pitchGrid

    else
        Set.remove ( pitchIdx, stepIdx ) pitchGrid


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


percPositionToTuple : PercPos -> ( Int, Int )
percPositionToTuple { percType, stepIdx } =
    ( Instruments.percRowIdx percType, stepIdx )


-- Calculations


getTotalSteps : TimeConfig -> Int
getTotalSteps config =
    config.bars * config.beatsPerBar * config.subdivisions


noteDuration : TimeConfig -> Float
noteDuration config =
    -- 60 seconds/minute ÷ BPM = seconds per beat
    -- Then divide by subdivisions = seconds per step
    (60.0 / toFloat config.bpm) / toFloat config.subdivisions


-- Grid Transformations


resizePitchGrid : ScaleConfig -> ScaleConfig -> TimeConfig -> PitchGrid -> PitchGrid
resizePitchGrid oldConfig newConfig newTimeConfig existingGrid =
    existingGrid
        |> Set.toList
        |> List.filterMap
            (\( pitchIdx, stepIdx ) ->
                let
                    midiPitch =
                        Scales.pitchIdxToMidi pitchIdx oldConfig
                in
                case Scales.midiToPitchIdx midiPitch newConfig of
                    Just newPitchIdx ->
                        if newPitchIdx < Scales.getTotalPitches newConfig && stepIdx < getTotalSteps newTimeConfig then
                            Just ( newPitchIdx, stepIdx )

                        else
                            Nothing

                    Nothing ->
                        Nothing
            )
        |> Set.fromList


transposePitchGrid : ScaleConfig -> ScaleConfig -> PitchGrid -> PitchGrid
transposePitchGrid oldConfig newConfig existingGrid =
    existingGrid
        |> Set.toList
        |> List.filterMap
            (\( pitchIdx, stepIdx ) ->
                let
                    scaleDegreeInfo =
                        Scales.pitchIdxToScaleDegree pitchIdx oldConfig
                in
                case Scales.scaleDegreeToPitchIdx scaleDegreeInfo newConfig of
                    Just newPitchIdx ->
                        Just ( newPitchIdx, stepIdx )

                    Nothing ->
                        Nothing
            )
        |> Set.fromList


-- Grid Conversions


convertMelodyToGrid : List (List String) -> ScaleConfig -> PitchGrid
convertMelodyToGrid stepMelodies config =
    stepMelodies
        |> List.indexedMap
            (\stepIdx noteNames ->
                List.filterMap
                    (\noteName ->
                        case Scales.noteNameToPitchIdx noteName config of
                            Just pitchIdx ->
                                Just ( pitchIdx, stepIdx )

                            Nothing ->
                                Nothing
                    )
                    noteNames
            )
        |> List.concat
        |> Set.fromList


convertPercussionToGrid : List (List PercType) -> PercGrid
convertPercussionToGrid stepPercussion =
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