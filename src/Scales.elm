module Scales exposing
    ( RootNote(..)
    , ScaleConfig
    , ScaleType(..)
    , allRootNotes
    , allScales
    , chromaticNoteNames
    , getRootNoteOffset
    , getScalePattern
    , getTotalPitches
    , midiToPitchIdx
    , noteNameToMidi
    , notesPerOctave
    , parseRootNote
    , parseScaleType
    , pitchIdxToMidi
    , pitchIdxToNoteName
    , rootNoteToString
    , scaleLabel
    )


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


getScalePattern : ScaleType -> List Int
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


getRootNoteOffset : RootNote -> Int
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


type alias ScaleConfig =
    { scaleType : ScaleType
    , rootNote : RootNote
    , octaveStart : Int
    , octaveCount : Int
    }



-- Scale-based Calculations


getTotalPitches : ScaleConfig -> Int
getTotalPitches config =
    notesPerOctave config.scaleType * config.octaveCount


pitchIdxToMidi : Int -> ScaleConfig -> Int
pitchIdxToMidi pitchIdx config =
    let
        scalePattern =
            getScalePattern config.scaleType

        rootOffset =
            getRootNoteOffset config.rootNote

        notesInScale =
            notesPerOctave config.scaleType

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

        midiC4 =
            60
    in
    if octaveIdx < config.octaveCount then
        baseC0 + (octave * 12) + rootOffset + semitone

    else
        midiC4


pitchIdxToNoteName : Int -> ScaleConfig -> String
pitchIdxToNoteName pitchIdx config =
    let
        scalePattern =
            getScalePattern config.scaleType

        rootOffset =
            getRootNoteOffset config.rootNote

        notesInScale =
            notesPerOctave config.scaleType

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
            Maybe.withDefault "?" (List.drop chromaticIndex chromaticNoteNames |> List.head)
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


noteNameToMidi : String -> Maybe Int
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
