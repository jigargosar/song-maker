module Scales exposing
    ( MidiNote
    , RootNote(..)
    , ScaleRange
    , ScaleType(..)
    , allRootNotes
    , allScales
    , getRootNoteOffset
    , noteNameToMidi
    , nthNoteName
    , nthNoteToMidi
    , parseRootNote
    , parseScaleType
    , rangeSize
    , rootNoteToString
    , scaleLabel
    , validateMidi
    )

import List.Extra as List
import Utils exposing (..)


type alias MidiNote =
    Int


type alias Semitone =
    Int


type alias OctaveIdx =
    Int


type ScaleType
    = Major
    | Pentatonic
    | Chromatic


allScales : List ScaleType
allScales =
    [ Major, Pentatonic, Chromatic ]


scaleLabel : ScaleType -> String
scaleLabel scale =
    case scale of
        Major ->
            "Major"

        Pentatonic ->
            "Pentatonic"

        Chromatic ->
            "Chromatic"


getScalePattern : ScaleType -> List Semitone
getScalePattern scaleType =
    case scaleType of
        Major ->
            [ 0, 2, 4, 5, 7, 9, 11 ]

        Pentatonic ->
            [ 0, 2, 4, 7, 9 ]

        Chromatic ->
            [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ]


type RootNote
    = C
    | CSharp
    | D
    | DSharp
    | E
    | F
    | FSharp
    | G
    | GSharp
    | A
    | ASharp
    | B


getRootNoteOffset : RootNote -> Semitone
getRootNoteOffset rootNote =
    case rootNote of
        C ->
            0

        CSharp ->
            1

        D ->
            2

        DSharp ->
            3

        E ->
            4

        F ->
            5

        FSharp ->
            6

        G ->
            7

        GSharp ->
            8

        A ->
            9

        ASharp ->
            10

        B ->
            11


chromaticNoteNames : List String
chromaticNoteNames =
    [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]


notesPerOctave : ScaleType -> Int
notesPerOctave scaleType =
    List.length (getScalePattern scaleType)


allRootNotes : List RootNote
allRootNotes =
    [ C, CSharp, D, DSharp, E, F, FSharp, G, GSharp, A, ASharp, B ]


rootNoteToString : RootNote -> String
rootNoteToString rootNote =
    case rootNote of
        C ->
            "C"

        CSharp ->
            "C#"

        D ->
            "D"

        DSharp ->
            "D#"

        E ->
            "E"

        F ->
            "F"

        FSharp ->
            "F#"

        G ->
            "G"

        GSharp ->
            "G#"

        A ->
            "A"

        ASharp ->
            "A#"

        B ->
            "B"


parseScaleType : String -> ScaleType
parseScaleType str =
    case str of
        "Major" ->
            Major

        "Pentatonic" ->
            Pentatonic

        "Chromatic" ->
            Chromatic

        _ ->
            Major


parseRootNote : String -> RootNote
parseRootNote str =
    case str of
        "C" ->
            C

        "C#" ->
            CSharp

        "D" ->
            D

        "D#" ->
            DSharp

        "E" ->
            E

        "F" ->
            F

        "F#" ->
            FSharp

        "G" ->
            G

        "G#" ->
            GSharp

        "A" ->
            A

        "A#" ->
            ASharp

        "B" ->
            B

        _ ->
            C



-- Scale Configuration


type alias ScaleRange =
    { scaleType : ScaleType
    , rootNote : RootNote
    , startingOctave : OctaveIdx
    , totalOctaves : Int
    }



-- Scale-based Calculations


rangeSize : ScaleRange -> Int
rangeSize config =
    notesPerOctave config.scaleType * config.totalOctaves


isWithinRange : Int -> ScaleRange -> Bool
isWithinRange noteIdx config =
    let
        notesInScale =
            notesPerOctave config.scaleType

        octaveIdx =
            noteIdx // notesInScale
    in
    octaveIdx < config.totalOctaves


absoluteOctave : Int -> ScaleRange -> Int
absoluteOctave noteIdx config =
    let
        notesInScale =
            notesPerOctave config.scaleType
    in
    config.startingOctave + (noteIdx // notesInScale)


semitoneInOctave : Int -> ScaleRange -> Semitone
semitoneInOctave noteIdx config =
    let
        notesInScale =
            notesPerOctave config.scaleType

        positionInScale =
            modBy notesInScale noteIdx

        rootOffset =
            getRootNoteOffset config.rootNote

        scaleOffset =
            getScalePattern config.scaleType
                |> List.getAt positionInScale
                |> Maybe.withDefault 0
    in
    rootOffset + scaleOffset


noteNameFromIdx : Int -> ScaleRange -> String
noteNameFromIdx noteIdx config =
    let
        semitone =
            semitoneInOctave noteIdx config

        chromaticIndex =
            modBy 12 semitone
    in
    chromaticNoteNames
        |> List.getAt chromaticIndex
        |> Maybe.withDefault "?"


nthNoteToMidi : Int -> ScaleRange -> MidiNote
nthNoteToMidi noteIdx config =
    if isWithinRange noteIdx config then
        12 + (absoluteOctave noteIdx config * 12) + semitoneInOctave noteIdx config

    else
        60


nthNoteName : Int -> ScaleRange -> String
nthNoteName noteIdx config =
    if isWithinRange noteIdx config then
        noteNameFromIdx noteIdx config ++ String.fromInt (absoluteOctave noteIdx config)

    else
        "C4"


validateMidi : MidiNote -> ScaleRange -> Maybe MidiNote
validateMidi targetMidi sc =
    rangeSize sc
        |> indices
        |> List.findMap (\pitchIdx -> justIf (nthNoteToMidi pitchIdx sc == targetMidi) targetMidi)


noteNameToMidi : String -> Maybe MidiNote
noteNameToMidi noteName =
    let
        parseNote str =
            case String.uncons str of
                Nothing ->
                    Nothing

                Just ( firstChar, rest ) ->
                    let
                        baseNote =
                            String.fromChar firstChar

                        modifier =
                            if String.startsWith "#" rest then
                                1

                            else
                                0

                        octaveStr =
                            String.dropLeft modifier rest

                        noteOffset =
                            case baseNote of
                                "C" ->
                                    Just 0

                                "D" ->
                                    Just 2

                                "E" ->
                                    Just 4

                                "F" ->
                                    Just 5

                                "G" ->
                                    Just 7

                                "A" ->
                                    Just 9

                                "B" ->
                                    Just 11

                                _ ->
                                    Nothing
                    in
                    Maybe.map2
                        (\offset octave -> (octave + 1) * 12 + offset + modifier)
                        noteOffset
                        (String.toInt octaveStr)
    in
    parseNote noteName
