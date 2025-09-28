module Grid exposing
    ( PitchPos
    , PitchGrid
    , PercPos
    , PercGrid
    , ScaleConfig
    , TimeConfig
    , getTotalPitches
    , getTotalSteps
    , pitchIdxToMidi
    , pitchIdxToNoteName
    , isPitchCellActive
    , updatePitchCell
    , isPercCellActive
    , updatePercCell
    )

import Instruments exposing (PercType)
import Scales exposing (RootNote, ScaleType)
import Set exposing (Set)
import Utils exposing (times)


-- Config Types


type alias ScaleConfig =
    { scaleType : ScaleType
    , rootNote : RootNote
    , octaveStart : Int
    , octaveCount : Int
    }


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


getTotalPitches : ScaleConfig -> Int
getTotalPitches config =
    Scales.notesPerOctave config.scaleType * config.octaveCount


getTotalSteps : TimeConfig -> Int
getTotalSteps config =
    config.bars * config.beatsPerBar * config.subdivisions


noteDuration : TimeConfig -> Float
noteDuration config =
    -- 60 seconds/minute ÷ BPM = seconds per beat
    -- Then divide by subdivisions = seconds per step
    (60.0 / toFloat config.bpm) / toFloat config.subdivisions


pitchIdxToMidi : Int -> ScaleConfig -> Int
pitchIdxToMidi pitchIdx config =
    let
        scalePattern =
            Scales.getScalePattern config.scaleType

        rootOffset =
            Scales.getRootNoteOffset config.rootNote

        notesInScale =
            Scales.notesPerOctave config.scaleType

        octaveIdx =
            pitchIdx // notesInScale

        noteIdx =
            modBy notesInScale pitchIdx

        octave =
            config.octaveStart + octaveIdx

        semitone =
            Maybe.withDefault 0 (List.drop noteIdx scalePattern |> List.head)

        baseC0 =
            12
    in
    if octaveIdx < config.octaveCount then
        baseC0 + (octave * 12) + rootOffset + semitone

    else
        midiC4


pitchIdxToNoteName : Int -> ScaleConfig -> String
pitchIdxToNoteName pitchIdx config =
    let
        scalePattern =
            Scales.getScalePattern config.scaleType

        rootOffset =
            Scales.getRootNoteOffset config.rootNote

        notesInScale =
            Scales.notesPerOctave config.scaleType

        octaveIdx =
            pitchIdx // notesInScale

        noteIdx =
            modBy notesInScale pitchIdx

        octave =
            config.octaveStart + octaveIdx

        semitone =
            Maybe.withDefault 0 (List.drop noteIdx scalePattern |> List.head)

        chromaticIndex =
            modBy 12 (rootOffset + semitone)

        noteName =
            Maybe.withDefault "?" (List.drop chromaticIndex Scales.chromaticNoteNames |> List.head)
    in
    if octaveIdx < config.octaveCount then
        noteName ++ String.fromInt octave

    else
        "C4"


midiToPitchIdx : Int -> ScaleConfig -> Maybe Int
midiToPitchIdx targetMidi config =
    let
        totalPitches =
            getTotalPitches config
    in
    List.range 0 (totalPitches - 1)
        |> List.filter (\pitchIdx -> pitchIdxToMidi pitchIdx config == targetMidi)
        |> List.head


noteNameToPitchIdx : String -> ScaleConfig -> Maybe Int
noteNameToPitchIdx noteName config =
    let
        totalPitches =
            getTotalPitches config
    in
    List.range 0 (totalPitches - 1)
        |> List.filter (\pitchIdx -> pitchIdxToNoteName pitchIdx config == noteName)
        |> List.head


pitchIdxToScaleDegree : Int -> ScaleConfig -> { scaleDegree : Int, octave : Int }
pitchIdxToScaleDegree pitchIdx config =
    let
        notesInScale =
            Scales.notesPerOctave config.scaleType

        octaveIdx =
            pitchIdx // notesInScale

        noteIdx =
            modBy notesInScale pitchIdx

        absoluteOctave =
            config.octaveStart + octaveIdx
    in
    { scaleDegree = noteIdx, octave = absoluteOctave }


scaleDegreeToPitchIdx : { scaleDegree : Int, octave : Int } -> ScaleConfig -> Maybe Int
scaleDegreeToPitchIdx { scaleDegree, octave } config =
    let
        notesInScale =
            Scales.notesPerOctave config.scaleType

        octaveIdx =
            octave - config.octaveStart
    in
    if octaveIdx >= 0 && octaveIdx < config.octaveCount && scaleDegree >= 0 && scaleDegree < notesInScale then
        Just (octaveIdx * notesInScale + scaleDegree)

    else
        Nothing


-- Grid Transformations


resizePitchGrid : ScaleConfig -> ScaleConfig -> TimeConfig -> PitchGrid -> PitchGrid
resizePitchGrid oldConfig newConfig newTimeConfig existingGrid =
    existingGrid
        |> Set.toList
        |> List.filterMap
            (\( pitchIdx, stepIdx ) ->
                let
                    midiPitch =
                        pitchIdxToMidi pitchIdx oldConfig
                in
                case midiToPitchIdx midiPitch newConfig of
                    Just newPitchIdx ->
                        if newPitchIdx < getTotalPitches newConfig && stepIdx < getTotalSteps newTimeConfig then
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
                        pitchIdxToScaleDegree pitchIdx oldConfig
                in
                case scaleDegreeToPitchIdx scaleDegreeInfo newConfig of
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
                        case noteNameToPitchIdx noteName config of
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