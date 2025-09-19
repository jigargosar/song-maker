module Types exposing
    ( ScaleType(..)
    , RootNote(..)
    , getScalePattern
    , getRootNoteOffset
    , getScaleNoteNames
    , scaleTypeToString
    , stringToScaleType
    , rootNoteToString
    , stringToRootNote
    , allScaleTypes
    , allRootNotes
    , notesPerOctave
    )


type ScaleType
    = Major
    | Pentatonic
    | Chromatic


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


chromaticNoteNames : List String
chromaticNoteNames =
    [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]


getScalePattern : ScaleType -> List Int
getScalePattern scaleType =
    case scaleType of
        Major ->
            [ 0, 2, 4, 5, 7, 9, 11 ]

        Pentatonic ->
            [ 0, 2, 4, 7, 9 ]

        Chromatic ->
            [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ]


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


getScaleNoteNames : ScaleType -> RootNote -> List String
getScaleNoteNames scaleType rootNote =
    let
        rootOffset =
            getRootNoteOffset rootNote

        scalePattern =
            getScalePattern scaleType
    in
    List.map
        (\semitoneOffset ->
            let
                chromaticIndex =
                    modBy 12 (rootOffset + semitoneOffset)
            in
            Maybe.withDefault "?" (List.drop chromaticIndex chromaticNoteNames |> List.head)
        )
        scalePattern


scaleTypeToString : ScaleType -> String
scaleTypeToString scaleType =
    case scaleType of
        Major ->
            "Major"

        Pentatonic ->
            "Pentatonic"

        Chromatic ->
            "Chromatic"


stringToScaleType : String -> ScaleType
stringToScaleType str =
    case str of
        "Major" ->
            Major

        "Pentatonic" ->
            Pentatonic

        "Chromatic" ->
            Chromatic

        _ ->
            Major


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


stringToRootNote : String -> RootNote
stringToRootNote str =
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


allScaleTypes : List ScaleType
allScaleTypes =
    [ Major, Pentatonic, Chromatic ]


allRootNotes : List RootNote
allRootNotes =
    [ C, CSharp, D, DSharp, E, F, FSharp, G, GSharp, A, ASharp, B ]


notesPerOctave : ScaleType -> Int
notesPerOctave scaleType =
    List.length (getScalePattern scaleType)