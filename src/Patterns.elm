module Patterns exposing
    ( PatternConfig
    , twinkleTwinkleChordsConfig
    , twinkleTwinkleChordsV2Config
    , twinkleTwinkleChordsV3Config
    , twinkleTwinkleConfig
    , vShapeConfig
    )

import Types exposing (ScaleType(..), RootNote(..))

-- COMMON TYPES AND FUNCTIONS


type alias PatternConfig =
    { grid : List (List Bool)
    , bpm : Int
    , barCount : Int
    , beatsPerBar : Int
    , splitBeats : Int
    , name : String
    , octaveStart : Int
    , octaveCount : Int
    , scaleType : ScaleType
    , rootNote : RootNote
    }


notesToGrid :
    { stepsWithNotes : List (List String)
    , octaveStart : Int
    , octaveCount : Int
    , stepCount : Int
    }
    -> List (List Bool)
notesToGrid o =
    let
        noteCount_ =
            o.octaveCount * 7

        emptyGridLocal =
            List.repeat noteCount_ (List.repeat o.stepCount False)

        parseNote : String -> Maybe ( String, Int )
        parseNote noteStr =
            case String.toList noteStr of
                [] ->
                    Nothing

                [ _ ] ->
                    Nothing

                noteLetter :: octaveChars ->
                    case String.fromList octaveChars |> String.toInt of
                        Just octave ->
                            Just ( String.fromChar noteLetter, octave )

                        Nothing ->
                            Nothing

        noteToGridIndex : String -> Maybe Int
        noteToGridIndex noteStr =
            case parseNote noteStr of
                Just ( noteLetter, octave ) ->
                    let
                        noteOffset =
                            case noteLetter of
                                "C" ->
                                    0

                                "D" ->
                                    1

                                "E" ->
                                    2

                                "F" ->
                                    3

                                "G" ->
                                    4

                                "A" ->
                                    5

                                "B" ->
                                    6

                                _ ->
                                    -1

                        octaveOffset =
                            octave - o.octaveStart

                        gridIndex =
                            octaveOffset * 7 + noteOffset
                    in
                    if noteOffset >= 0 && octaveOffset >= 0 && gridIndex < noteCount_ then
                        Just gridIndex

                    else
                        Nothing

                Nothing ->
                    Nothing

        setNoteAtStep : Int -> Int -> List (List Bool) -> List (List Bool)
        setNoteAtStep noteIndex stepIndex grid =
            List.indexedMap
                (\nIdx noteRow ->
                    if nIdx == noteIndex then
                        List.indexedMap
                            (\sIdx isActive ->
                                if sIdx == stepIndex then
                                    True

                                else
                                    isActive
                            )
                            noteRow

                    else
                        noteRow
                )
                grid

        processStep : ( Int, List String ) -> List (List Bool) -> List (List Bool)
        processStep ( stepIndex, noteNames ) grid =
            List.foldl
                (\noteName currentGrid ->
                    case noteToGridIndex noteName of
                        Just noteIndex ->
                            setNoteAtStep noteIndex stepIndex currentGrid

                        Nothing ->
                            currentGrid
                )
                grid
                noteNames
    in
    List.foldl processStep emptyGridLocal (List.indexedMap Tuple.pair o.stepsWithNotes)



-- ORDERED LIST OF PATTERNS


vShapePattern : List (List String)
vShapePattern =
    [ [ "C3" ]
    , [ "D3" ]
    , [ "E3" ]
    , [ "F3" ]
    , [ "G3" ]
    , [ "A3" ]
    , [ "B3" ]
    , [ "C4" ]
    , [ "C4" ]
    , [ "B3" ]
    , [ "A3" ]
    , [ "G3" ]
    , [ "F3" ]
    , [ "E3" ]
    , [ "D3" ]
    , [ "C3" ]
    ]


twinkleTwinklePattern : List (List String)
twinkleTwinklePattern =
    [ [ "C4" ]
    , [ "C4" ]
    , [ "G4" ]
    , [ "G4" ]
    , [ "A4" ]
    , [ "A4" ]
    , [ "G4" ]
    , []
    , [ "F4" ]
    , [ "F4" ]
    , [ "E4" ]
    , [ "E4" ]
    , [ "D4" ]
    , [ "D4" ]
    , [ "C4" ]
    , []
    , [ "G4" ]
    , [ "G4" ]
    , [ "F4" ]
    , [ "F4" ]
    , [ "E4" ]
    , [ "E4" ]
    , [ "D4" ]
    , []
    , [ "G4" ]
    , [ "G4" ]
    , [ "F4" ]
    , [ "F4" ]
    , [ "E4" ]
    , [ "E4" ]
    , [ "D4" ]
    , []
    , [ "C4" ]
    , [ "C4" ]
    , [ "G4" ]
    , [ "G4" ]
    , [ "A4" ]
    , [ "A4" ]
    , [ "G4" ]
    , []
    , [ "F4" ]
    , [ "F4" ]
    , [ "E4" ]
    , [ "E4" ]
    , [ "D4" ]
    , [ "D4" ]
    , [ "C4" ]
    , []
    , [ "G4" ]
    , [ "G4" ]
    , [ "F4" ]
    , [ "F4" ]
    , [ "E4" ]
    , [ "E4" ]
    , [ "D4" ]
    , []
    , [ "G4" ]
    , [ "G4" ]
    , [ "F4" ]
    , [ "F4" ]
    , [ "E4" ]
    , [ "E4" ]
    , [ "D4" ]
    , []
    ]


twinkleTwinkleChordsPattern : List (List String)
twinkleTwinkleChordsPattern =
    [ [ "C4", "E4", "G4" ]
    , [ "C4" ]
    , [ "G4" ]
    , [ "G4" ]
    , [ "A4", "C5", "E5" ]
    , [ "A4" ]
    , [ "G4" ]
    , []
    , [ "F4", "A4", "C5" ]
    , [ "F4" ]
    , [ "E4" ]
    , [ "E4" ]
    , [ "D4", "F4", "A4" ]
    , [ "D4" ]
    , [ "C4" ]
    , []
    , [ "G4", "B4", "D5" ]
    , [ "G4" ]
    , [ "F4" ]
    , [ "F4" ]
    , [ "E4", "G4", "C5" ]
    , [ "E4" ]
    , [ "D4" ]
    , []
    , [ "G4", "B4", "D5" ]
    , [ "G4" ]
    , [ "F4" ]
    , [ "F4" ]
    , [ "E4", "G4", "C5" ]
    , [ "E4" ]
    , [ "D4" ]
    , []
    , [ "C4", "E4", "G4" ]
    , [ "C4" ]
    , [ "G4" ]
    , [ "G4" ]
    , [ "A4", "C5", "E5" ]
    , [ "A4" ]
    , [ "G4" ]
    , []
    , [ "F4", "A4", "C5" ]
    , [ "F4" ]
    , [ "E4" ]
    , [ "E4" ]
    , [ "D4", "F4", "A4" ]
    , [ "D4" ]
    , [ "C4" ]
    , []
    , [ "G4", "B4", "D5" ]
    , [ "G4" ]
    , [ "F4" ]
    , [ "F4" ]
    , [ "E4", "G4", "C5" ]
    , [ "E4" ]
    , [ "D4" ]
    , []
    , [ "G4", "B4", "D5" ]
    , [ "G4" ]
    , [ "F4" ]
    , [ "F4" ]
    , [ "E4", "G4", "C5" ]
    , [ "E4" ]
    , [ "D4" ]
    , []
    ]


twinkleTwinkleChordsV2Pattern : List (List String)
twinkleTwinkleChordsV2Pattern =
    [ [ "C4", "C5", "E5", "G5" ]
    , [ "C4" ]
    , [ "G4", "G5", "B5", "D6" ]
    , [ "G4" ]
    , [ "A4", "A5", "C6", "E6" ]
    , [ "A4" ]
    , [ "G4", "G5", "B5", "D6" ]
    , []
    , [ "F4", "F5", "A5", "C6" ]
    , [ "F4" ]
    , [ "E4", "C5", "E5", "G5" ]
    , [ "E4" ]
    , [ "D4", "G5", "B5", "D6" ]
    , [ "D4" ]
    , [ "C4", "C5", "E5", "G5" ]
    , []
    , [ "G4", "G5", "B5", "D6" ]
    , [ "G4" ]
    , [ "F4", "F5", "A5", "C6" ]
    , [ "F4" ]
    , [ "E4", "C5", "E5", "G5" ]
    , [ "E4" ]
    , [ "D4", "G5", "B5", "D6" ]
    , []
    , [ "G4", "G5", "B5", "D6" ]
    , [ "G4" ]
    , [ "F4", "F5", "A5", "C6" ]
    , [ "F4" ]
    , [ "E4", "C5", "E5", "G5" ]
    , [ "E4" ]
    , [ "D4", "G5", "B5", "D6" ]
    , []
    , [ "C4", "C5", "E5", "G5" ]
    , [ "C4" ]
    , [ "G4", "G5", "B5", "D6" ]
    , [ "G4" ]
    , [ "A4", "A5", "C6", "E6" ]
    , [ "A4" ]
    , [ "G4", "G5", "B5", "D6" ]
    , []
    , [ "F4", "F5", "A5", "C6" ]
    , [ "F4" ]
    , [ "E4", "C5", "E5", "G5" ]
    , [ "E4" ]
    , [ "D4", "G5", "B5", "D6" ]
    , [ "D4" ]
    , [ "C4", "C5", "E5", "G5" ]
    , []
    , [ "G4", "G5", "B5", "D6" ]
    , [ "G4" ]
    , [ "F4", "F5", "A5", "C6" ]
    , [ "F4" ]
    , [ "E4", "C5", "E5", "G5" ]
    , [ "E4" ]
    , [ "D4", "G5", "B5", "D6" ]
    , []
    , [ "C4", "C5", "E5", "G5" ]
    , [ "C4" ]
    , [ "G4", "G5", "B5", "D6" ]
    , [ "G4" ]
    , [ "A4", "A5", "C6", "E6" ]
    , [ "A4" ]
    , [ "G4", "G5", "B5", "D6" ]
    , []
    , [ "F4", "F5", "A5", "C6" ]
    , [ "F4" ]
    , [ "E4", "C5", "E5", "G5" ]
    , [ "E4" ]
    , [ "D4", "G5", "B5", "D6" ]
    , [ "D4" ]
    , [ "C4", "C5", "E5", "G5" ]
    , []
    ]


twinkleTwinkleChordsV3Pattern : List (List String)
twinkleTwinkleChordsV3Pattern =
    [ [ "C4", "E4", "G4", "C5" ]
    , [ "C4" ]
    , [ "G4", "B4", "D5", "G5" ]
    , [ "G4" ]
    , [ "A4", "C5", "E5", "A5" ]
    , [ "A4" ]
    , [ "G4", "B4", "D5", "G5" ]
    , []
    , [ "F4", "A4", "C5", "F5" ]
    , [ "F4" ]
    , [ "E4", "G4", "C5", "E5" ]
    , [ "E4" ]
    , [ "D4", "G4", "B4", "D5" ]
    , [ "D4" ]
    , [ "C4", "E4", "G4", "C5" ]
    , []
    , [ "G4", "B4", "D5", "G5" ]
    , [ "G4" ]
    , [ "F4", "A4", "C5", "F5" ]
    , [ "F4" ]
    , [ "E4", "G4", "C5", "E5" ]
    , [ "E4" ]
    , [ "D4", "G4", "B4", "D5" ]
    , []
    , [ "G4", "B4", "D5", "G5" ]
    , [ "G4" ]
    , [ "F4", "A4", "C5", "F5" ]
    , [ "F4" ]
    , [ "E4", "G4", "C5", "E5" ]
    , [ "E4" ]
    , [ "D4", "G4", "B4", "D5" ]
    , []
    , [ "C4", "E4", "G4", "C5" ]
    , [ "C4" ]
    , [ "G4", "B4", "D5", "G5" ]
    , [ "G4" ]
    , [ "A4", "C5", "E5", "A5" ]
    , [ "A4" ]
    , [ "G4", "B4", "D5", "G5" ]
    , []
    , [ "F4", "A4", "C5", "F5" ]
    , [ "F4" ]
    , [ "E4", "G4", "C5", "E5" ]
    , [ "E4" ]
    , [ "D4", "G4", "B4", "D5" ]
    , [ "D4" ]
    , [ "C4", "E4", "G4", "C5" ]
    , []
    ]



-- ORDERED LIST OF CONFIGS


vShapeConfig : PatternConfig
vShapeConfig =
    let
        octaveStart =
            3

        octaveCount =
            3

        stepCount =
            List.length vShapePattern
    in
    { grid =
        notesToGrid
            { stepsWithNotes = vShapePattern
            , octaveStart = octaveStart
            , octaveCount = octaveCount
            , stepCount = stepCount
            }
    , bpm = 120
    , barCount = 4
    , beatsPerBar = 2
    , splitBeats = 2
    , name = "V-Shape"
    , octaveStart = octaveStart
    , octaveCount = octaveCount
    , scaleType = Major
    , rootNote = C
    }


twinkleTwinkleConfig : PatternConfig
twinkleTwinkleConfig =
    let
        octaveStart =
            4

        octaveCount =
            1

        stepCount =
            List.length twinkleTwinklePattern
    in
    { grid =
        notesToGrid
            { stepsWithNotes = twinkleTwinklePattern
            , octaveStart = octaveStart
            , octaveCount = octaveCount
            , stepCount = stepCount
            }
    , bpm = 120
    , barCount = 16
    , beatsPerBar = 2
    , splitBeats = 2
    , name = "Twinkle Twinkle"
    , octaveStart = octaveStart
    , octaveCount = octaveCount
    , scaleType = Major
    , rootNote = C
    }


twinkleTwinkleChordsConfig : PatternConfig
twinkleTwinkleChordsConfig =
    let
        octaveStart =
            4

        octaveCount =
            2

        stepCount =
            List.length twinkleTwinkleChordsPattern
    in
    { grid =
        notesToGrid
            { stepsWithNotes = twinkleTwinkleChordsPattern
            , octaveStart = octaveStart
            , octaveCount = octaveCount
            , stepCount = stepCount
            }
    , bpm = 120
    , barCount = 16
    , beatsPerBar = 2
    , splitBeats = 2
    , name = "Twinkle Twinkle Chords"
    , octaveStart = octaveStart
    , octaveCount = octaveCount
    , scaleType = Major
    , rootNote = C
    }


twinkleTwinkleChordsV2Config : PatternConfig
twinkleTwinkleChordsV2Config =
    let
        octaveStart =
            4

        octaveCount =
            2

        stepCount =
            List.length twinkleTwinkleChordsV2Pattern
    in
    { grid =
        notesToGrid
            { stepsWithNotes = twinkleTwinkleChordsV2Pattern
            , octaveStart = octaveStart
            , octaveCount = octaveCount
            , stepCount = stepCount
            }
    , bpm = 120
    , barCount = 20
    , beatsPerBar = 2
    , splitBeats = 2
    , name = "Twinkle Chords V2"
    , octaveStart = octaveStart
    , octaveCount = octaveCount
    , scaleType = Major
    , rootNote = C
    }


twinkleTwinkleChordsV3Config : PatternConfig
twinkleTwinkleChordsV3Config =
    let
        octaveStart =
            4

        octaveCount =
            2

        stepCount =
            List.length twinkleTwinkleChordsV3Pattern
    in
    { grid =
        notesToGrid
            { stepsWithNotes = twinkleTwinkleChordsV3Pattern
            , octaveStart = octaveStart
            , octaveCount = octaveCount
            , stepCount = stepCount
            }
    , bpm = 120
    , barCount = 13
    , beatsPerBar = 2
    , splitBeats = 2
    , name = "Twinkle Chords V3"
    , octaveStart = octaveStart
    , octaveCount = octaveCount
    , scaleType = Major
    , rootNote = C
    }
