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
    , resizePitchGrid
    , setPercCell
    , setPitchCell
    , shiftStepRight
    , transposePitchGrid
    )

import Instruments exposing (PercType)
import Scales exposing (RootNote, ScaleConfig)
import Set exposing (Set)
import Timing exposing (TimeConfig)
import Utils exposing (..)



-- Grid Types


type alias PitchPos =
    { pitchIdx : Int, stepIdx : Int }


type alias MidiNote =
    Int


type alias StepIdx =
    Int


type alias PitchGrid =
    Set ( MidiNote, StepIdx )


type alias PercPos =
    { percType : PercType, stepIdx : Int }


type alias PercGrid =
    Set ( Int, Int )



-- Basic Grid Operations


initialPitchGrid : PitchGrid
initialPitchGrid =
    Set.empty


initialPercGrid : PercGrid
initialPercGrid =
    Set.empty


isPitchCellActive : PitchPos -> ScaleConfig -> PitchGrid -> Bool
isPitchCellActive position config pitchGrid =
    Set.member (pitchPositionToTuple position config) pitchGrid


setPitchCell : PitchPos -> ScaleConfig -> Bool -> PitchGrid -> PitchGrid
setPitchCell position config isActive =
    setUpdate isActive (pitchPositionToTuple position config)


pitchPositionToTuple : PitchPos -> ScaleConfig -> ( Int, Int )
pitchPositionToTuple { pitchIdx, stepIdx } config =
    ( Scales.pitchIdxToMidi pitchIdx config, stepIdx )


isPercCellActive : PercPos -> PercGrid -> Bool
isPercCellActive position grid =
    Set.member (percPositionToTuple position) grid


setPercCell : PercPos -> Bool -> PercGrid -> PercGrid
setPercCell position isActive =
    setUpdate isActive (percPositionToTuple position)


percPositionToTuple : PercPos -> ( Int, Int )
percPositionToTuple { percType, stepIdx } =
    ( Instruments.percRowIdx percType, stepIdx )



-- Internal Helpers (not exposed)


gridShift : Int -> Int -> Set ( Int, Int ) -> Set ( Int, Int )
gridShift fromStepIdx totalSteps =
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


gridDeleteStep : Int -> Int -> Set ( Int, Int ) -> Set ( Int, Int )
gridDeleteStep stepToDelete totalSteps grid =
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


gridToString : Set ( Int, Int ) -> String
gridToString grid =
    grid
        |> Set.toList
        |> List.concatMap (\( row, step ) -> [ String.fromInt row, String.fromInt step ])
        |> String.join ","


parseGrid : String -> Set ( Int, Int )
parseGrid str =
    str
        |> String.split ","
        |> List.filterMap String.toInt
        |> pairUp
        |> Set.fromList



-- Grid Transformations


resizePitchGrid : ScaleConfig -> TimeConfig -> PitchGrid -> PitchGrid
resizePitchGrid newConfig newTimeConfig =
    setFilterMap
        (\( midiNote, stepIdx ) ->
            -- Check if MIDI note is valid in new scale and step is within bounds
            case Scales.midiToPitchIdx midiNote newConfig of
                Just _ ->
                    if stepIdx < Timing.getTotalSteps newTimeConfig then
                        Just ( midiNote, stepIdx )

                    else
                        Nothing

                Nothing ->
                    Nothing
        )


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
    ( gridShift fromStepIdx totalSteps pitchGrid
    , gridShift fromStepIdx totalSteps percGrid
    )


deleteStep : Int -> Int -> PitchGrid -> PercGrid -> ( PitchGrid, PercGrid )
deleteStep stepToDelete totalSteps pitchGrid percGrid =
    ( gridDeleteStep stepToDelete totalSteps pitchGrid
    , gridDeleteStep stepToDelete totalSteps percGrid
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
    gridToString


{-| Parse comma-separated integers back to PitchGrid: "0,0,1,2,5,10"
-}
parsePitchGrid : String -> PitchGrid
parsePitchGrid =
    parseGrid


{-| Convert PercGrid to comma-separated integers: "0,0,1,2,5,10"
-}
percGridToString : PercGrid -> String
percGridToString =
    gridToString


{-| Parse comma-separated integers back to PercGrid: "0,0,1,2,5,10"
-}
parsePercGrid : String -> PercGrid
parsePercGrid =
    parseGrid


{-| Group list into pairs: [0,0,1,2] -> [(0,0),(1,2)]
-}
pairUp : List Int -> List ( Int, Int )
pairUp list =
    case list of
        x :: y :: rest ->
            pairUp rest
                |> (\pairs -> ( x, y ) :: pairs)

        _ ->
            []
