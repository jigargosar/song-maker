module Grid exposing
    ( PercGrid
    , PercPos
    , PitchGrid
    , PitchPos
    , convertMelodyToGrid
    , convertPercussionToGrid
    , deleteStep
    , initialPercGrid
    , initialPitchGrid
    , isPercCellActive
    , isPitchCellActive
    , parsePercGrid
    , parsePitchGrid
    , percGridToString
    , pitchGridToString
    , pitchPos
    , resizePitchGrid
    , setPercCell
    , setPitchCell
    , shiftStepRight
    , transposePitchGrid
    )

import Instruments exposing (PercType)
import Internal.Grid as InternalGrid
import Scales exposing (MidiNote, PitchIdx, RootNote, ScaleConfig)
import Set exposing (Set)
import Timing exposing (StepIdx, TimeConfig)
import Utils exposing (..)



-- Grid Types


type alias PitchGrid =
    Set PitchCell


type alias PitchCell =
    ( MidiNote, StepIdx )


type alias PitchPos =
    { pitchIdx : PitchIdx, stepIdx : StepIdx }


pitchPos : PitchIdx -> StepIdx -> PitchPos
pitchPos pitchIdx stepIdx =
    { pitchIdx = pitchIdx, stepIdx = stepIdx }


type alias PercGrid =
    Set ( PercRowIdx, StepIdx )


type alias PercRowIdx =
    Int


type alias PercPos =
    { percType : PercType, stepIdx : StepIdx }



-- Basic Grid Operations


initialPitchGrid : PitchGrid
initialPitchGrid =
    Set.empty


initialPercGrid : PercGrid
initialPercGrid =
    Set.empty


isPitchCellActive : PitchPos -> ScaleConfig -> PitchGrid -> Bool
isPitchCellActive position config pitchGrid =
    InternalGrid.isActive (pitchPositionToTuple position config) pitchGrid


setPitchCell : PitchPos -> ScaleConfig -> Bool -> PitchGrid -> PitchGrid
setPitchCell position config isActive pitchGrid =
    InternalGrid.setCell (pitchPositionToTuple position config) isActive pitchGrid


pitchPositionToTuple : PitchPos -> ScaleConfig -> ( MidiNote, StepIdx )
pitchPositionToTuple { pitchIdx, stepIdx } config =
    ( Scales.pitchIdxToMidi pitchIdx config, stepIdx )


isPercCellActive : PercPos -> PercGrid -> Bool
isPercCellActive position grid =
    InternalGrid.isActive (percPositionToTuple position) grid


setPercCell : PercPos -> Bool -> PercGrid -> PercGrid
setPercCell position isActive grid =
    InternalGrid.setCell (percPositionToTuple position) isActive grid


percPositionToTuple : PercPos -> ( PercRowIdx, StepIdx )
percPositionToTuple { percType, stepIdx } =
    ( Instruments.percRowIdx percType, stepIdx )



-- Grid Transformations


resizePitchGrid : ScaleConfig -> TimeConfig -> PitchGrid -> PitchGrid
resizePitchGrid sc tc =
    let
        validateCell : PitchCell -> Maybe PitchCell
        validateCell ( midiNote, stepIdx ) =
            Maybe.map2 Tuple.pair (Scales.validateMidi midiNote sc) (Timing.validateStep stepIdx tc)
    in
    setFilterMap validateCell


transposePitchGrid : { prev : RootNote, next : RootNote } -> PitchGrid -> PitchGrid
transposePitchGrid { prev, next } existingGrid =
    let
        -- Calculate semitone difference between old and new root
        semitonesDelta =
            Scales.getRootNoteOffset next - Scales.getRootNoteOffset prev
    in
    existingGrid
        |> Set.map (\( midiNote, stepIdx ) -> ( midiNote + semitonesDelta, stepIdx ))


shiftStepRight : Int -> Int -> PitchGrid -> PercGrid -> ( PitchGrid, PercGrid )
shiftStepRight fromStepIdx totalSteps pitchGrid percGrid =
    ( InternalGrid.shift fromStepIdx totalSteps pitchGrid
    , InternalGrid.shift fromStepIdx totalSteps percGrid
    )


deleteStep : Int -> Int -> PitchGrid -> PercGrid -> ( PitchGrid, PercGrid )
deleteStep stepToDelete totalSteps pitchGrid percGrid =
    ( InternalGrid.delete stepToDelete totalSteps pitchGrid
    , InternalGrid.delete stepToDelete totalSteps percGrid
    )



-- Grid Conversions


convertMelodyToGrid : List (List String) -> PitchGrid
convertMelodyToGrid stepMelodies =
    stepMelodies
        |> List.indexedMap
            (\stepIdx noteNames ->
                List.filterMap
                    (\noteName ->
                        Scales.noteNameToMidi noteName
                            |> Maybe.map (\midiNote -> ( midiNote, stepIdx ))
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



-- Grid Serialization


{-| Convert PitchGrid to comma-separated integers: "0,0,1,2,5,10"
-}
pitchGridToString : PitchGrid -> String
pitchGridToString =
    InternalGrid.toString


{-| Parse comma-separated integers back to PitchGrid: "0,0,1,2,5,10"
-}
parsePitchGrid : String -> PitchGrid
parsePitchGrid =
    InternalGrid.fromString


{-| Convert PercGrid to comma-separated integers: "0,0,1,2,5,10"
-}
percGridToString : PercGrid -> String
percGridToString =
    InternalGrid.toString


{-| Parse comma-separated integers back to PercGrid: "0,0,1,2,5,10"
-}
parsePercGrid : String -> PercGrid
parsePercGrid =
    InternalGrid.fromString
