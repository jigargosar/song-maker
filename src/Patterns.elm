module Patterns exposing
    ( PatternConfig
    , twinkleTwinkleChordsConfig
    , twinkleTwinkleChordsV2Config
    , twinkleTwinkleChordsV3Config
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
-- Twinkle Twinkle V2: cleaner chord arrangement with brighter harmony
-- Twinkle Twinkle V3: chords always above C4, no low-pitch chords


twinkleTwinkleV3Pattern : List (List String)
twinkleTwinkleV3Pattern =
    [ [ "C4", "E4", "G4", "C5" ] -- Twin-kle
    , [ "C4" ] -- twin-kle
    , [ "G4", "B4", "D5", "G5" ] -- lit-tle
    , [ "G4" ] -- star
    , [ "A4", "C5", "E5", "A5" ] -- How
    , [ "A4" ] -- I
    , [ "G4", "B4", "D5", "G5" ] -- won-der
    , [] -- what
    , [ "F4", "A4", "C5", "F5" ] -- you
    , [ "F4" ] -- are
    , [ "E4", "G4", "C5", "E5" ] --
    , [ "E4" ] --
    , [ "D4", "G4", "B4", "D5" ] --
    , [ "D4" ] --
    , [ "C4", "E4", "G4", "C5" ] --
    , [] --
    , [ "G4", "B4", "D5", "G5" ] -- Up
    , [ "G4" ] -- a-
    , [ "F4", "A4", "C5", "F5" ] -- bove
    , [ "F4" ] -- the
    , [ "E4", "G4", "C5", "E5" ] -- world
    , [ "E4" ] -- so
    , [ "D4", "G4", "B4", "D5" ] -- high
    , [] --
    , [ "G4", "B4", "D5", "G5" ] -- Like
    , [ "G4" ] -- a
    , [ "F4", "A4", "C5", "F5" ] -- dia-
    , [ "F4" ] -- mond
    , [ "E4", "G4", "C5", "E5" ] -- in
    , [ "E4" ] -- the
    , [ "D4", "G4", "B4", "D5" ] -- sky
    , [] --
    , [ "C4", "E4", "G4", "C5" ] -- Twin-kle
    , [ "C4" ] -- twin-kle
    , [ "G4", "B4", "D5", "G5" ] -- lit-tle
    , [ "G4" ] -- star
    , [ "A4", "C5", "E5", "A5" ] -- How
    , [ "A4" ] -- I
    , [ "G4", "B4", "D5", "G5" ] -- won-der
    , [] -- what
    , [ "F4", "A4", "C5", "F5" ] -- you
    , [ "F4" ] -- are
    , [ "E4", "G4", "C5", "E5" ] --
    , [ "E4" ] --
    , [ "D4", "G4", "B4", "D5" ] --
    , [ "D4" ] --
    , [ "C4", "E4", "G4", "C5" ] --
    , [] --
    ]


twinkleTwinkleV2Pattern : List (List String)
twinkleTwinkleV2Pattern =
    [ [ "C4", "C5", "E5", "G5" ] -- Twin-kle
    , [ "C4" ] -- twin-kle
    , [ "G4", "G5", "B5", "D6" ] -- lit-tle
    , [ "G4" ] -- star
    , [ "A4", "A5", "C6", "E6" ] -- How
    , [ "A4" ] -- I
    , [ "G4", "G5", "B5", "D6" ] -- won-der
    , [] -- what
    , [ "F4", "F5", "A5", "C6" ] -- you
    , [ "F4" ] -- are
    , [ "E4", "C5", "E5", "G5" ] --
    , [ "E4" ] --
    , [ "D4", "G5", "B5", "D6" ] --
    , [ "D4" ] --
    , [ "C4", "C5", "E5", "G5" ] --
    , [] --
    , [ "G4", "G5", "B5", "D6" ] -- Up
    , [ "G4" ] -- a-
    , [ "F4", "F5", "A5", "C6" ] -- bove
    , [ "F4" ] -- the
    , [ "E4", "C5", "E5", "G5" ] -- world
    , [ "E4" ] -- so
    , [ "D4", "G5", "B5", "D6" ] -- high
    , [] --
    , [ "G4", "G5", "B5", "D6" ] -- Like
    , [ "G4" ] -- a
    , [ "F4", "F5", "A5", "C6" ] -- dia-
    , [ "F4" ] -- mond
    , [ "E4", "C5", "E5", "G5" ] -- in
    , [ "E4" ] -- the
    , [ "D4", "G5", "B5", "D6" ] -- sky
    , [] --
    , [ "C4", "C5", "E5", "G5" ] -- Twin-kle
    , [ "C4" ] -- twin-kle
    , [ "G4", "G5", "B5", "D6" ] -- lit-tle
    , [ "G4" ] -- star
    , [ "A4", "A5", "C6", "E6" ] -- How
    , [ "A4" ] -- I
    , [ "G4", "G5", "B5", "D6" ] -- won-der
    , [] -- what
    , [ "F4", "F5", "A5", "C6" ] -- you
    , [ "F4" ] -- are
    , [ "E4", "C5", "E5", "G5" ] --
    , [ "E4" ] --
    , [ "D4", "G5", "B5", "D6" ] --
    , [ "D4" ] --
    , [ "C4", "C5", "E5", "G5" ] --
    , [] --
    , [ "G4", "G5", "B5", "D6" ] -- When
    , [ "G4" ] -- the
    , [ "F4", "F5", "A5", "C6" ] -- bla-
    , [ "F4" ] -- zing
    , [ "E4", "C5", "E5", "G5" ] -- sun
    , [ "E4" ] -- is
    , [ "D4", "G5", "B5", "D6" ] -- gone
    , [] --
    , [ "G4", "G5", "B5", "D6" ] -- When
    , [ "G4" ] -- he
    , [ "F4", "F5", "A5", "C6" ] -- no-
    , [ "F4" ] -- thing
    , [ "E4", "C5", "E5", "G5" ] -- shines
    , [ "E4" ] -- up-
    , [ "D4", "G5", "B5", "D6" ] -- on
    , [] --
    , [ "C4", "C5", "E5", "G5" ] -- Then
    , [ "C4" ] -- you
    , [ "G4", "G5", "B5", "D6" ] -- show
    , [ "G4" ] -- your
    , [ "A4", "A5", "C6", "E6" ] -- lit-
    , [ "A4" ] -- tle
    , [ "G4", "G5", "B5", "D6" ] -- light
    , [] --
    , [ "F4", "F5", "A5", "C6" ] -- Twin-
    , [ "F4" ] -- kle
    , [ "E4", "C5", "E5", "G5" ] -- twin-
    , [ "E4" ] -- kle
    , [ "D4", "G5", "B5", "D6" ] -- all
    , [ "D4" ] -- the
    , [ "C4", "C5", "E5", "G5" ] -- night
    , [] --
    ]


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


twinkleTwinkleChordsV3Config : PatternConfig
twinkleTwinkleChordsV3Config =
    let
        octaveStart =
            4

        octaveCount =
            2

        stepCount =
            List.length twinkleTwinkleV3Pattern
    in
    { grid =
        notesToGrid
            { stepsWithNotes = twinkleTwinkleV3Pattern
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
    }


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
    }


twinkleTwinkleChordsV2Config : PatternConfig
twinkleTwinkleChordsV2Config =
    let
        octaveStart =
            4

        octaveCount =
            2

        stepCount =
            List.length twinkleTwinkleV2Pattern
    in
    { grid =
        notesToGrid
            { stepsWithNotes = twinkleTwinkleV2Pattern
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
    }
