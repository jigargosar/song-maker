module Scale exposing
    ( RootNote(..)
    , ScaleType(..)
    , allRootNotes
    , chromaticNoteNames
    , getRootNoteOffset
    , getScalePattern
    , getTotalPitches
    , notesPerOctave
    , parseRootNote
    , parseScaleType
    , pitchIdxToMidi
    , pitchIdxToNoteName
    , rootNoteToString
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


parseScaleType : String -> Maybe ScaleType
parseScaleType str =
    case str of
        "Major" ->
            Just Major

        "Pentatonic" ->
            Just Pentatonic

        "Chromatic" ->
            Just Chromatic

        _ ->
            Nothing


parseRootNote : String -> Maybe RootNote
parseRootNote str =
    case str of
        "C" ->
            Just C

        "C#" ->
            Just CSharp

        "D" ->
            Just D

        "D#" ->
            Just DSharp

        "E" ->
            Just E

        "F" ->
            Just F

        "F#" ->
            Just FSharp

        "G" ->
            Just G

        "G#" ->
            Just GSharp

        "A" ->
            Just A

        "A#" ->
            Just ASharp

        "B" ->
            Just B

        _ ->
            Nothing


getTotalPitches : ScaleType -> { start : Int, count : Int } -> Int
getTotalPitches scaleType octaveRange =
    notesPerOctave scaleType * octaveRange.count


pitchIdxToMidi : Int -> ScaleType -> RootNote -> { start : Int, count : Int } -> Int
pitchIdxToMidi pitchIdx scaleType rootNote octaveRange =
    let
        scalePattern =
            getScalePattern scaleType

        rootOffset =
            getRootNoteOffset rootNote

        notesInScale =
            notesPerOctave scaleType

        octaveIdx =
            pitchIdx // notesInScale

        noteIdx =
            modBy notesInScale pitchIdx

        octave =
            octaveRange.start + octaveIdx

        semitone =
            Maybe.withDefault 0 (List.drop noteIdx scalePattern |> List.head)

        baseC0 =
            12
    in
    if octaveIdx < octaveRange.count then
        baseC0 + (octave * 12) + rootOffset + semitone

    else
        60



-- midiC4 fallback


pitchIdxToNoteName : Int -> ScaleType -> RootNote -> { start : Int, count : Int } -> String
pitchIdxToNoteName pitchIdx scaleType rootNote octaveRange =
    let
        scalePattern =
            getScalePattern scaleType

        rootOffset =
            getRootNoteOffset rootNote

        notesInScale =
            notesPerOctave scaleType

        octaveIdx =
            pitchIdx // notesInScale

        noteIdx =
            modBy notesInScale pitchIdx

        octave =
            octaveRange.start + octaveIdx

        semitone =
            Maybe.withDefault 0 (List.drop noteIdx scalePattern |> List.head)

        chromaticIndex =
            modBy 12 (rootOffset + semitone)

        noteName =
            Maybe.withDefault "?" (List.drop chromaticIndex chromaticNoteNames |> List.head)
    in
    if octaveIdx < octaveRange.count then
        noteName ++ String.fromInt octave

    else
        "C4"
