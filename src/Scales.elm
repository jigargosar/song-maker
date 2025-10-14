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


scaleInfo : ScaleType -> { label : String, pattern : List Semitone, noteCount : Int }
scaleInfo scaleType =
    case scaleType of
        Major ->
            { label = "Major", pattern = [ 0, 2, 4, 5, 7, 9, 11 ], noteCount = 7 }

        Pentatonic ->
            { label = "Pentatonic", pattern = [ 0, 2, 4, 7, 9 ], noteCount = 5 }

        Chromatic ->
            { label = "Chromatic", pattern = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ], noteCount = 12 }


scaleLabel : ScaleType -> String
scaleLabel scaleType =
    (scaleInfo scaleType).label


getScalePattern : ScaleType -> List Semitone
getScalePattern scaleType =
    (scaleInfo scaleType).pattern


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


allRootNotes : List RootNote
allRootNotes =
    [ C, CSharp, D, DSharp, E, F, FSharp, G, GSharp, A, ASharp, B ]


noteOffsets : List Semitone
noteOffsets =
    List.range 0 11


noteData : List ( String, RootNote, Semitone )
noteData =
    List.zip3 chromaticNoteNames allRootNotes noteOffsets


getRootNoteOffset : RootNote -> Semitone
getRootNoteOffset rootNote =
    List.findMap (\( _, note, offset ) -> justIf (note == rootNote) offset) noteData
        |> Maybe.withDefault 0


notesPerOctave : ScaleType -> Int
notesPerOctave scaleType =
    (scaleInfo scaleType).noteCount


rootNoteToString : RootNote -> String
rootNoteToString rootNote =
    List.findMap (\( name, note, _ ) -> justIf (note == rootNote) name) noteData
        |> Maybe.withDefault "C"


parseScaleType : String -> ScaleType
parseScaleType str =
    List.findMap (\st -> justIf (scaleLabel st == str) st) allScales
        |> Maybe.withDefault Major


parseRootNote : String -> RootNote
parseRootNote str =
    List.findMap (\( name, note, _ ) -> justIf (name == str) note) noteData
        |> Maybe.withDefault C



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


{-| Parse a note name to MIDI note number.

Well-formed inputs:
noteNameToMidi "C4" == Just 60
noteNameToMidi "C#10" == Just 145

Malformed inputs return Nothing:
noteNameToMidi "C##2" == Nothing
noteNameToMidi "C" == Nothing

-}
noteNameToMidi : String -> Maybe MidiNote
noteNameToMidi noteName =
    let
        -- Extract all digits from the string (e.g., "C#10" -> "10")
        octaveStr =
            String.filter Char.isDigit noteName

        -- Count how many characters to drop from the right (e.g., "10" -> 2)
        octaveLen =
            String.length octaveStr

        -- Get the note part by removing octave digits (e.g., "C#10" -> "C#")
        note =
            String.dropRight octaveLen noteName
    in
    Maybe.map2
        (\offset octave -> (octave + 1) * 12 + offset)
        (List.elemIndex note chromaticNoteNames)
        (String.toInt octaveStr)
