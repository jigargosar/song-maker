module Scales exposing
    ( RootNote
    , ScaleType
    , allRootNotes
    , allScales
    , chromaticNoteNames
    , getRootNoteOffset
    , getScalePattern
    , major
    , notesPerOctave
    , parseRootNote
    , parseScaleType
    , root
    , rootNoteToString
    , scaleLabel
    )


major =
    Major


root =
    C


type ScaleType
    = Major
    | Pentatonic
    | Chromatic


allScales =
    [ Major, Pentatonic, Chromatic ]


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
