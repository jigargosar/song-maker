module Patterns exposing
    ( PatternConfig
    , twinkleTwinkleChordsConfig
    , twinkleTwinkleConfig
    , vShapeConfig
    )

-- PATTERN CONFIGURATION TYPE


type alias PatternConfig =
    { grid : List (List Bool)
    , bpm : Int
    , barCount : Int
    , beatsPerBar : Int
    , splitBeats : Int
    , name : String
    , octaveStart : Int
    , octaveCount : Int
    }



-- GRID GENERATION FUNCTIONS


toggleGridCell : Int -> Int -> List (List Bool) -> List (List Bool)
toggleGridCell noteIndex stepIndex grid =
    List.indexedMap
        (\nIdx noteRow ->
            if nIdx == noteIndex then
                List.indexedMap
                    (\sIdx isActive ->
                        if sIdx == stepIndex then
                            not isActive

                        else
                            isActive
                    )
                    noteRow

            else
                noteRow
        )
        grid


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

        setNoteAtStep : String -> Int -> List (List Bool) -> List (List Bool)
        setNoteAtStep noteName stepIndex grid =
            case noteToGridIndex noteName of
                Just noteIndex ->
                    toggleGridCell noteIndex stepIndex grid

                Nothing ->
                    grid

        processStep : Int -> List String -> List (List Bool) -> List (List Bool)
        processStep stepIndex noteNames grid =
            List.foldl (\noteName currentGrid -> setNoteAtStep noteName stepIndex currentGrid) grid noteNames
    in
    List.indexedMap processStep o.stepsWithNotes
        |> List.foldl (\stepProcessor currentGrid -> stepProcessor currentGrid) emptyGridLocal



-- PATTERN DATA
-- V-shaped melody demo: smooth stepwise progression C3 -> C4 -> C3


vShapePattern : List (List String)
vShapePattern =
    [ [ "C3" ]
    , [ "D3" ]
    , [ "E3" ]
    , [ "F3" ]
    , [ "G3" ]
    , [ "A3" ]
    , [ "B3" ]
    , [ "C4" ] -- Ascending
    , [ "C4" ]
    , [ "B3" ]
    , [ "A3" ]
    , [ "G3" ]
    , [ "F3" ]
    , [ "E3" ]
    , [ "D3" ]
    , [ "C3" ] -- Descending
    ]



-- Twinkle Twinkle Little Star full melody pattern (64 steps)


twinkleTwinklePattern : List (List String)
twinkleTwinklePattern =
    [ [ "C4" ]
    , [ "C4" ]
    , [ "G4" ]
    , [ "G4" ] -- Twinkle twinkle
    , [ "A4" ]
    , [ "A4" ]
    , [ "G4" ]
    , [] -- little star
    , [ "F4" ]
    , [ "F4" ]
    , [ "E4" ]
    , [ "E4" ] -- How I wonder
    , [ "D4" ]
    , [ "D4" ]
    , [ "C4" ]
    , [] -- what you are
    , [ "G4" ]
    , [ "G4" ]
    , [ "F4" ]
    , [ "F4" ] -- Up above the
    , [ "E4" ]
    , [ "E4" ]
    , [ "D4" ]
    , [] -- world so high
    , [ "G4" ]
    , [ "G4" ]
    , [ "F4" ]
    , [ "F4" ] -- Like a diamond
    , [ "E4" ]
    , [ "E4" ]
    , [ "D4" ]
    , [] -- in the sky
    , [ "C4" ]
    , [ "C4" ]
    , [ "G4" ]
    , [ "G4" ] -- Twinkle twinkle (repeat)
    , [ "A4" ]
    , [ "A4" ]
    , [ "G4" ]
    , [] -- little star
    , [ "F4" ]
    , [ "F4" ]
    , [ "E4" ]
    , [ "E4" ] -- How I wonder
    , [ "D4" ]
    , [ "D4" ]
    , [ "C4" ]
    , [] -- what you are
    , [ "G4" ]
    , [ "G4" ]
    , [ "F4" ]
    , [ "F4" ] -- Up above the
    , [ "E4" ]
    , [ "E4" ]
    , [ "D4" ]
    , [] -- world so high
    , [ "G4" ]
    , [ "G4" ]
    , [ "F4" ]
    , [ "F4" ] -- Like a diamond
    , [ "E4" ]
    , [ "E4" ]
    , [ "D4" ]
    , [] -- in the sky
    ]



-- Twinkle Twinkle Little Star with chords (chords only on key steps, 64 steps)


twinkleTwinkleChordsPattern : List (List String)
twinkleTwinkleChordsPattern =
    [ [ "C4", "E4", "G4" ] -- chord (bar 1)
    , [ "C4" ]
    , [ "G4" ]
    , [ "G4" ]
    , [ "A4", "C5", "E5" ] -- chord (bar 2)
    , [ "A4" ]
    , [ "G4" ]
    , []
    , [ "F4", "A4", "C5" ] -- chord (bar 3)
    , [ "F4" ]
    , [ "E4" ]
    , [ "E4" ]
    , [ "D4", "F4", "A4" ] -- chord (bar 4)
    , [ "D4" ]
    , [ "C4" ]
    , []
    , [ "G4", "B4", "D5" ] -- chord (bar 5)
    , [ "G4" ]
    , [ "F4" ]
    , [ "F4" ]
    , [ "E4", "G4", "C5" ] -- chord (bar 6)
    , [ "E4" ]
    , [ "D4" ]
    , []
    , [ "G4", "B4", "D5" ] -- chord (bar 7)
    , [ "G4" ]
    , [ "F4" ]
    , [ "F4" ]
    , [ "E4", "G4", "C5" ] -- chord (bar 8)
    , [ "E4" ]
    , [ "D4" ]
    , []
    , [ "C4", "E4", "G4" ] -- chord (bar 9)
    , [ "C4" ]
    , [ "G4" ]
    , [ "G4" ]
    , [ "A4", "C5", "E5" ] -- chord (bar 10)
    , [ "A4" ]
    , [ "G4" ]
    , []
    , [ "F4", "A4", "C5" ] -- chord (bar 11)
    , [ "F4" ]
    , [ "E4" ]
    , [ "E4" ]
    , [ "D4", "F4", "A4" ] -- chord (bar 12)
    , [ "D4" ]
    , [ "C4" ]
    , []
    , [ "G4", "B4", "D5" ] -- chord (bar 13)
    , [ "G4" ]
    , [ "F4" ]
    , [ "F4" ]
    , [ "E4", "G4", "C5" ] -- chord (bar 14)
    , [ "E4" ]
    , [ "D4" ]
    , []
    , [ "G4", "B4", "D5" ] -- chord (bar 15)
    , [ "G4" ]
    , [ "F4" ]
    , [ "F4" ]
    , [ "E4", "G4", "C5" ] -- chord (bar 16)
    , [ "E4" ]
    , [ "D4" ]
    , []
    ]



-- PATTERN CONFIGURATIONS


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
    }


twinkleTwinkleConfig : PatternConfig
twinkleTwinkleConfig =
    let
        octaveStart =
            3

        octaveCount =
            3

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
    }


twinkleTwinkleChordsConfig : PatternConfig
twinkleTwinkleChordsConfig =
    let
        octaveStart =
            3

        octaveCount =
            3

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
    }
