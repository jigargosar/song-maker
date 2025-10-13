module TonalGrid exposing
    ( PitchPos
    , RootNote
    , ScaleType
    , TonalGrid
    , changeRoot
    , changeScaleType
    , changeStartingOctave
    , changeTotalOctaves
    , deleteStep
    , fromMelody
    , getRootNote
    , getScaleType
    , getStartingOctave
    , getTotalOctaves
    , getTotalPitches
    , initial
    , isActive
    , parse
    , pitchPos
    , pitchIdxToMidi
    , pitchIdxToNoteName
    , resize
    , serialize
    , setCell
    , shiftStepRight
    )

import Internal.Grid as InternalGrid
import Scales
import Set exposing (Set)
import Timing exposing (StepIdx, TimeConfig)
import Utils exposing (..)


type TonalGrid
    = TonalGrid InternalState


type alias RootNote =
    Scales.RootNote


type alias ScaleType =
    Scales.ScaleType


type alias InternalState =
    { cells : Set PitchCell
    , scaleType : ScaleType
    , rootNote : RootNote
    , startingOctave : Int
    , totalOctaves : Int
    }


type alias PitchCell =
    ( Scales.MidiNote, StepIdx )


type alias PitchPos =
    { pitchIdx : Scales.PitchIdx, stepIdx : StepIdx }


pitchPos : Scales.PitchIdx -> StepIdx -> PitchPos
pitchPos pitchIdx stepIdx =
    { pitchIdx = pitchIdx, stepIdx = stepIdx }



-- Basic Grid Operations


initial : ScaleType -> RootNote -> Int -> Int -> TonalGrid
initial scaleType rootNote startingOctave totalOctaves =
    TonalGrid
        { cells = Set.empty
        , scaleType = scaleType
        , rootNote = rootNote
        , startingOctave = startingOctave
        , totalOctaves = totalOctaves
        }


isActive : PitchPos -> TonalGrid -> Bool
isActive position (TonalGrid state) =
    InternalGrid.get (positionToTuple position state) state.cells


setCell : PitchPos -> Bool -> TonalGrid -> TonalGrid
setCell position isActive_ (TonalGrid state) =
    TonalGrid { state | cells = InternalGrid.set (positionToTuple position state) isActive_ state.cells }


positionToTuple : PitchPos -> InternalState -> ( Scales.MidiNote, StepIdx )
positionToTuple { pitchIdx, stepIdx } state =
    ( Scales.pitchIdxToMidi pitchIdx (toScaleConfig state), stepIdx )


toScaleConfig : InternalState -> Scales.ScaleConfig
toScaleConfig state =
    { scaleType = state.scaleType
    , rootNote = state.rootNote
    , startingOctave = state.startingOctave
    , totalOctaves = state.totalOctaves
    }



-- Grid Transformations


resize : TimeConfig -> TonalGrid -> TonalGrid
resize tc (TonalGrid state) =
    let
        sc =
            toScaleConfig state

        validateCell : PitchCell -> Maybe PitchCell
        validateCell ( midiNote, stepIdx ) =
            Maybe.map2 Tuple.pair (Scales.validateMidi midiNote sc) (Timing.validateStep stepIdx tc)
    in
    TonalGrid { state | cells = setFilterMap validateCell state.cells }


shiftStepRight : Int -> Int -> TonalGrid -> TonalGrid
shiftStepRight fromStepIdx totalSteps (TonalGrid state) =
    TonalGrid { state | cells = InternalGrid.shiftColumnRight fromStepIdx totalSteps state.cells }


deleteStep : Int -> Int -> TonalGrid -> TonalGrid
deleteStep stepToDelete totalSteps (TonalGrid state) =
    TonalGrid { state | cells = InternalGrid.deleteColumn stepToDelete totalSteps state.cells }



-- Scale Configuration Queries


getScaleType : TonalGrid -> ScaleType
getScaleType (TonalGrid state) =
    state.scaleType


getRootNote : TonalGrid -> RootNote
getRootNote (TonalGrid state) =
    state.rootNote


getStartingOctave : TonalGrid -> Int
getStartingOctave (TonalGrid state) =
    state.startingOctave


getTotalOctaves : TonalGrid -> Int
getTotalOctaves (TonalGrid state) =
    state.totalOctaves


getTotalPitches : TonalGrid -> Int
getTotalPitches (TonalGrid state) =
    Scales.getTotalPitches (toScaleConfig state)



-- Scale Configuration Mutations


changeScaleType : ScaleType -> TimeConfig -> TonalGrid -> TonalGrid
changeScaleType newScaleType tc (TonalGrid state) =
    TonalGrid { state | scaleType = newScaleType }
        |> resize tc


changeRoot : RootNote -> TonalGrid -> TonalGrid
changeRoot newRoot (TonalGrid state) =
    let
        semitonesDelta =
            Scales.getRootNoteOffset newRoot - Scales.getRootNoteOffset state.rootNote

        transposedCells =
            Set.map (\( midiNote, stepIdx ) -> ( midiNote + semitonesDelta, stepIdx )) state.cells
    in
    TonalGrid { state | rootNote = newRoot, cells = transposedCells }


changeStartingOctave : Int -> TimeConfig -> TonalGrid -> TonalGrid
changeStartingOctave newOctave tc (TonalGrid state) =
    TonalGrid { state | startingOctave = newOctave }
        |> resize tc


changeTotalOctaves : Int -> TimeConfig -> TonalGrid -> TonalGrid
changeTotalOctaves newCount tc (TonalGrid state) =
    TonalGrid { state | totalOctaves = newCount }
        |> resize tc



-- Conversion Functions


pitchIdxToMidi : Scales.PitchIdx -> TonalGrid -> Scales.MidiNote
pitchIdxToMidi pitchIdx (TonalGrid state) =
    Scales.pitchIdxToMidi pitchIdx (toScaleConfig state)


pitchIdxToNoteName : Scales.PitchIdx -> TonalGrid -> String
pitchIdxToNoteName pitchIdx (TonalGrid state) =
    Scales.pitchIdxToNoteName pitchIdx (toScaleConfig state)



-- Grid Conversions


fromMelody : List (List String) -> ScaleType -> RootNote -> Int -> Int -> TonalGrid
fromMelody stepMelodies scaleType rootNote startingOctave totalOctaves =
    let
        cells =
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
    in
    TonalGrid
        { cells = cells
        , scaleType = scaleType
        , rootNote = rootNote
        , startingOctave = startingOctave
        , totalOctaves = totalOctaves
        }



-- Grid Serialization


{-| Convert TonalGrid to comma-separated integers: "0,0,1,2,5,10"
-}
serialize : TonalGrid -> String
serialize (TonalGrid state) =
    InternalGrid.serialize state.cells


{-| Parse comma-separated integers back to TonalGrid: "0,0,1,2,5,10"
-}
parse : String -> ScaleType -> RootNote -> Int -> Int -> TonalGrid
parse str scaleType rootNote startingOctave totalOctaves =
    TonalGrid
        { cells = InternalGrid.parse str
        , scaleType = scaleType
        , rootNote = rootNote
        , startingOctave = startingOctave
        , totalOctaves = totalOctaves
        }
