module TonalGrid exposing
    ( PitchPos
    , TonalGrid
    , deleteStep
    , fromMelody
    , initial
    , isActive
    , parse
    , pitchPos
    , resize
    , serialize
    , setCell
    , shiftStepRight
    , transpose
    )

import Internal.Grid as InternalGrid
import Scales exposing (MidiNote, RootNote, ScaleRange)
import Set exposing (Set)
import Timing exposing (StepIdx, TimeConfig)
import Utils exposing (..)


type alias TonalGrid =
    Set PitchCell


type alias PitchCell =
    ( MidiNote, StepIdx )


type alias PitchPos =
    { pitchIdx : Int, stepIdx : StepIdx }


pitchPos : Int -> StepIdx -> PitchPos
pitchPos pitchIdx stepIdx =
    { pitchIdx = pitchIdx, stepIdx = stepIdx }



-- Basic Grid Operations


initial : TonalGrid
initial =
    Set.empty


isActive : PitchPos -> ScaleRange -> TonalGrid -> Bool
isActive position config grid =
    InternalGrid.get (positionToTuple position config) grid


setCell : PitchPos -> ScaleRange -> Bool -> TonalGrid -> TonalGrid
setCell position config isActive_ grid =
    InternalGrid.set (positionToTuple position config) isActive_ grid


positionToTuple : PitchPos -> ScaleRange -> ( MidiNote, StepIdx )
positionToTuple { pitchIdx, stepIdx } config =
    ( Scales.nthNoteInRangeToMidi pitchIdx config, stepIdx )



-- Grid Transformations


resize : ScaleRange -> TimeConfig -> TonalGrid -> TonalGrid
resize sc tc =
    let
        validateCell : PitchCell -> Maybe PitchCell
        validateCell ( midiNote, stepIdx ) =
            Maybe.map2 Tuple.pair (Scales.validateMidi midiNote sc) (Timing.validateStep stepIdx tc)
    in
    setFilterMap validateCell


transpose : { prev : RootNote, next : RootNote } -> TonalGrid -> TonalGrid
transpose { prev, next } existingGrid =
    let
        -- Calculate semitone difference between old and new root
        semitonesDelta =
            Scales.getRootNoteOffset next - Scales.getRootNoteOffset prev
    in
    existingGrid
        |> Set.map (\( midiNote, stepIdx ) -> ( midiNote + semitonesDelta, stepIdx ))


shiftStepRight : Int -> Int -> TonalGrid -> TonalGrid
shiftStepRight fromStepIdx totalSteps grid =
    InternalGrid.shiftColumnRight fromStepIdx totalSteps grid


deleteStep : Int -> Int -> TonalGrid -> TonalGrid
deleteStep stepToDelete totalSteps grid =
    InternalGrid.deleteColumn stepToDelete totalSteps grid



-- Grid Conversions


fromMelody : List (List String) -> TonalGrid
fromMelody stepMelodies =
    stepMelodies
        |> List.indexedMap
            (\stepIdx noteNames ->
                noteNames
                    |> List.filterMap Scales.parseNoteNameToMidi
                    |> List.map (\midiNote -> ( midiNote, stepIdx ))
            )
        |> List.concat
        |> Set.fromList



-- Grid Serialization


{-| Convert TonalGrid to comma-separated integers: "0,0,1,2,5,10"
-}
serialize : TonalGrid -> String
serialize =
    InternalGrid.serialize


{-| Parse comma-separated integers back to TonalGrid: "0,0,1,2,5,10"
-}
parse : String -> TonalGrid
parse =
    InternalGrid.parse
