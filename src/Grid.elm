module Grid exposing
    ( PercGrid
    , PercPos
    , PitchGrid
    , PitchPos
    , convertMelodyToGrid
    , convertPercussionToGrid
    , empty
    , getTotalPitches
    , getTotalSteps
    , isPercCellActive
    , isPitchCellActive
    , pitchIdxToMidi
    , pitchIdxToNoteName
    , resizePitchGrid
    , transposePitchGrid
    , updatePercCell
    , updatePitchCell
    )

import Instruments exposing (PercType)
import Scales exposing (RootNote, ScaleType)
import Set exposing (Set)
import Utils exposing (times)



-- Types


type alias PitchPos =
    { pitchIdx : Int, stepIdx : Int }


type alias PitchGrid =
    Set ( Int, Int )


type alias PercPos =
    { percType : PercType, stepIdx : Int }


type alias PercGrid =
    Set ( Int, Int )


empty =
    Set.empty



-- Conversion Functions


percPositionToTuple : PercPos -> ( Int, Int )
percPositionToTuple { percType, stepIdx } =
    ( Instruments.percRowIdx percType, stepIdx )



-- Cell State Management


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



-- Percussion Grid Conversion


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



-- Grid Functions?


getTotalPitches : { a | scaleType : ScaleType, octaveCount : Int } -> Int
getTotalPitches model =
    Scales.notesPerOctave model.scaleType * model.octaveCount


getTotalSteps : { a | bars : Int, beatsPerBar : Int, beatSubdivisions : Int } -> Int
getTotalSteps c =
    c.bars * c.beatsPerBar * c.beatSubdivisions



pitchIdxToMidi :
    Int
    ->
        { a
            | scaleType : ScaleType
            , rootNote : RootNote
            , octaveStart : Int
            , octaveCount : Int
        }
    -> Int
pitchIdxToMidi pitchIdx model =
    let
        scalePattern =
            Scales.getScalePattern model.scaleType

        rootOffset =
            Scales.getRootNoteOffset model.rootNote

        notesInScale =
            Scales.notesPerOctave model.scaleType

        octaveIdx =
            pitchIdx // notesInScale

        noteIdx =
            modBy notesInScale pitchIdx

        octave =
            model.octaveStart + octaveIdx

        semitone =
            Maybe.withDefault 0 (List.drop noteIdx scalePattern |> List.head)

        baseC0 =
            12
    in
    if octaveIdx < model.octaveCount then
        baseC0 + (octave * 12) + rootOffset + semitone

    else
        midiC4


midiC4 : Int
midiC4 =
    60


midiToPitchIdx :
    Int
    ->
        { a
            | scaleType : ScaleType
            , rootNote : RootNote
            , octaveStart : Int
            , octaveCount : Int
        }
    -> Maybe Int
midiToPitchIdx targetMidi model =
    let
        totalPitches =
            getTotalPitches model
    in
    List.range 0 (totalPitches - 1)
        |> List.filter (\pitchIdx -> pitchIdxToMidi pitchIdx model == targetMidi)
        |> List.head


resizePitchGrid :
    { a
        | scaleType : ScaleType
        , rootNote : RootNote
        , octaveStart : Int
        , octaveCount : Int
    }
    ->
        { a
            | scaleType : ScaleType
            , rootNote : RootNote
            , octaveStart : Int
            , octaveCount : Int
            , bars : Int
            , beatsPerBar : Int
            , beatSubdivisions : Int
        }
    -> PitchGrid
    -> PitchGrid
resizePitchGrid oldModel newModel existingGrid =
    existingGrid
        |> Set.toList
        |> List.filterMap
            (\( pitchIdx, stepIdx ) ->
                let
                    midiPitch =
                        pitchIdxToMidi pitchIdx oldModel
                in
                case midiToPitchIdx midiPitch newModel of
                    Just newPitchIdx ->
                        if newPitchIdx < getTotalPitches newModel && stepIdx < getTotalSteps newModel then
                            Just ( newPitchIdx, stepIdx )

                        else
                            Nothing

                    Nothing ->
                        Nothing
            )
        |> Set.fromList


pitchIdxToNoteName :
    Int
    ->
        { a
            | scaleType : ScaleType
            , rootNote : RootNote
            , octaveStart : Int
            , octaveCount : Int
        }
    -> String
pitchIdxToNoteName pitchIdx model =
    let
        scalePattern =
            Scales.getScalePattern model.scaleType

        rootOffset =
            Scales.getRootNoteOffset model.rootNote

        notesInScale =
            Scales.notesPerOctave model.scaleType

        octaveIdx =
            pitchIdx // notesInScale

        noteIdx =
            modBy notesInScale pitchIdx

        octave =
            model.octaveStart + octaveIdx

        semitone =
            Maybe.withDefault 0 (List.drop noteIdx scalePattern |> List.head)

        chromaticIndex =
            modBy 12 (rootOffset + semitone)

        noteName =
            Maybe.withDefault "?" (List.drop chromaticIndex Scales.chromaticNoteNames |> List.head)
    in
    if octaveIdx < model.octaveCount then
        noteName ++ String.fromInt octave

    else
        "C4"


noteNameToPitchIdx :
    String
    ->
        { a
            | scaleType : ScaleType
            , rootNote : RootNote
            , octaveStart : Int
            , octaveCount : Int
        }
    -> Maybe Int
noteNameToPitchIdx noteName model =
    let
        totalPitches =
            getTotalPitches model
    in
    List.range 0 (totalPitches - 1)
        |> List.filter (\pitchIdx -> pitchIdxToNoteName pitchIdx model == noteName)
        |> List.head


convertMelodyToGrid :
    List (List String)
    ->
        { a
            | scaleType : ScaleType
            , rootNote : RootNote
            , octaveStart : Int
            , octaveCount : Int
        }
    -> PitchGrid
convertMelodyToGrid stepMelodies model =
    stepMelodies
        |> List.indexedMap
            (\stepIdx noteNames ->
                List.filterMap
                    (\noteName ->
                        let
                            pitchIdxMaybe =
                                noteNameToPitchIdx noteName model
                        in
                        case pitchIdxMaybe of
                            Just pitchIdx ->
                                Just ( pitchIdx, stepIdx )

                            Nothing ->
                                Nothing
                    )
                    noteNames
            )
        |> List.concat
        |> Set.fromList


{-| Transpose pitch grid preserving scale degree relationships
-}
transposePitchGrid :
    { a
        | scaleType : ScaleType
        , octaveStart : Int
    }
    ->
        { a
            | scaleType : ScaleType
            , octaveStart : Int
            , octaveCount : Int
        }
    -> PitchGrid
    -> PitchGrid
transposePitchGrid oldModel newModel existingGrid =
    existingGrid
        |> Set.toList
        |> List.filterMap
            (\( pitchIdx, stepIdx ) ->
                let
                    scaleDegreeInfo =
                        pitchIdxToScaleDegree pitchIdx oldModel
                in
                case scaleDegreeToPitchIdx scaleDegreeInfo newModel of
                    Just newPitchIdx ->
                        Just ( newPitchIdx, stepIdx )

                    Nothing ->
                        Nothing
            )
        |> Set.fromList


{-| Convert pitch index to scale degree and octave relative to current root/scale
-}
pitchIdxToScaleDegree :
    Int
    ->
        { a
            | scaleType : ScaleType
            , octaveStart : Int
        }
    -> { scaleDegree : Int, octave : Int }
pitchIdxToScaleDegree pitchIdx model =
    let
        notesInScale =
            Scales.notesPerOctave model.scaleType

        octaveIdx =
            pitchIdx // notesInScale

        noteIdx =
            modBy notesInScale pitchIdx

        absoluteOctave =
            model.octaveStart + octaveIdx
    in
    { scaleDegree = noteIdx, octave = absoluteOctave }


{-| Convert scale degree and octave to pitch index in target model
-}
scaleDegreeToPitchIdx :
    { scaleDegree : Int, octave : Int }
    ->
        { a
            | scaleType : ScaleType
            , octaveStart : Int
            , octaveCount : Int
        }
    -> Maybe Int
scaleDegreeToPitchIdx { scaleDegree, octave } model =
    let
        notesInScale =
            Scales.notesPerOctave model.scaleType

        octaveIdx =
            octave - model.octaveStart
    in
    if octaveIdx >= 0 && octaveIdx < model.octaveCount && scaleDegree >= 0 && scaleDegree < notesInScale then
        Just (octaveIdx * notesInScale + scaleDegree)

    else
        Nothing
