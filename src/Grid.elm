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
    , shiftStepRight
    , transposePitchGrid
    , updatePercCell
    , updatePitchCell
    )

import Instruments exposing (PercType)
import Scales exposing (ScaleConfig)
import Set exposing (Set)
import Timing exposing (TimeConfig)



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
isPitchCellActive { pitchIdx, stepIdx } config pitchGrid =
    let
        midiNote =
            Scales.pitchIdxToMidi pitchIdx config
    in
    Set.member ( midiNote, stepIdx ) pitchGrid


updatePitchCell : PitchPos -> ScaleConfig -> Bool -> PitchGrid -> PitchGrid
updatePitchCell { pitchIdx, stepIdx } config isActive pitchGrid =
    let
        midiNote =
            Scales.pitchIdxToMidi pitchIdx config
    in
    if isActive then
        Set.insert ( midiNote, stepIdx ) pitchGrid

    else
        Set.remove ( midiNote, stepIdx ) pitchGrid


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



-- Grid Transformations


resizePitchGrid : ScaleConfig -> TimeConfig -> PitchGrid -> PitchGrid
resizePitchGrid newConfig newTimeConfig existingGrid =
    existingGrid
        |> Set.toList
        |> List.filterMap
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
        |> Set.fromList


transposePitchGrid : ScaleConfig -> ScaleConfig -> PitchGrid -> PitchGrid
transposePitchGrid oldConfig newConfig existingGrid =
    let
        -- Calculate semitone difference between old and new root
        semitonesDelta =
            Scales.getRootNoteOffset newConfig.rootNote - Scales.getRootNoteOffset oldConfig.rootNote
    in
    existingGrid
        |> Set.map (\( midiNote, stepIdx ) -> ( midiNote + semitonesDelta, stepIdx ))


shiftStepRight : Int -> Int -> PitchGrid -> PercGrid -> ( PitchGrid, PercGrid )
shiftStepRight fromStepIdx totalSteps pitchGrid percGrid =
    let
        newPitchGrid =
            pitchGrid
                |> Set.toList
                |> List.filterMap
                    (\( midiNote, stepIdx ) ->
                        if stepIdx >= fromStepIdx then
                            let
                                newStepIdx =
                                    stepIdx + 1
                            in
                            if newStepIdx < totalSteps then
                                Just ( midiNote, newStepIdx )

                            else
                                Nothing

                        else
                            Just ( midiNote, stepIdx )
                    )
                |> Set.fromList

        newPercGrid =
            percGrid
                |> Set.toList
                |> List.filterMap
                    (\( percRowIdx, stepIdx ) ->
                        if stepIdx >= fromStepIdx then
                            let
                                newStepIdx =
                                    stepIdx + 1
                            in
                            if newStepIdx < totalSteps then
                                Just ( percRowIdx, newStepIdx )

                            else
                                Nothing

                        else
                            Just ( percRowIdx, stepIdx )
                    )
                |> Set.fromList
    in
    ( newPitchGrid, newPercGrid )


deleteStep : Int -> Int -> PitchGrid -> PercGrid -> ( PitchGrid, PercGrid )
deleteStep stepToDelete totalSteps pitchGrid percGrid =
    if stepToDelete < 0 || stepToDelete >= totalSteps then
        ( pitchGrid, percGrid )

    else
        let
            newPitchGrid =
                pitchGrid
                    |> Set.toList
                    |> List.filterMap
                        (\( midiNote, stepIdx ) ->
                            if stepIdx == stepToDelete then
                                Nothing

                            else if stepIdx > stepToDelete then
                                Just ( midiNote, stepIdx - 1 )

                            else
                                Just ( midiNote, stepIdx )
                        )
                    |> Set.fromList

            newPercGrid =
                percGrid
                    |> Set.toList
                    |> List.filterMap
                        (\( percRowIdx, stepIdx ) ->
                            if stepIdx == stepToDelete then
                                Nothing

                            else if stepIdx > stepToDelete then
                                Just ( percRowIdx, stepIdx - 1 )

                            else
                                Just ( percRowIdx, stepIdx )
                        )
                    |> Set.fromList
        in
        ( newPitchGrid, newPercGrid )



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
pitchGridToString grid =
    grid
        |> Set.toList
        |> List.concatMap (\( pitch, step ) -> [ String.fromInt pitch, String.fromInt step ])
        |> String.join ","


{-| Parse comma-separated integers back to PitchGrid: "0,0,1,2,5,10"
-}
parsePitchGrid : String -> Maybe PitchGrid
parsePitchGrid str =
    if String.isEmpty str then
        Just Set.empty

    else
        str
            |> String.split ","
            |> List.filterMap String.toInt
            |> pairUp
            |> Maybe.map Set.fromList


{-| Convert PercGrid to comma-separated integers: "0,0,1,2,5,10"
-}
percGridToString : PercGrid -> String
percGridToString grid =
    grid
        |> Set.toList
        |> List.concatMap (\( perc, step ) -> [ String.fromInt perc, String.fromInt step ])
        |> String.join ","


{-| Parse comma-separated integers back to PercGrid: "0,0,1,2,5,10"
-}
parsePercGrid : String -> Maybe PercGrid
parsePercGrid str =
    if String.isEmpty str then
        Just Set.empty

    else
        str
            |> String.split ","
            |> List.filterMap String.toInt
            |> pairUp
            |> Maybe.map Set.fromList


{-| Group list into pairs: [0,0,1,2] -> [(0,0),(1,2)]
-}
pairUp : List Int -> Maybe (List ( Int, Int ))
pairUp list =
    case list of
        [] ->
            Just []

        x :: y :: rest ->
            pairUp rest
                |> Maybe.map (\pairs -> ( x, y ) :: pairs)

        _ ->
            Nothing
