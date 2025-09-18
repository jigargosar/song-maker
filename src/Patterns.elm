module Patterns exposing
    ( vShapePattern
    , twinkleTwinklePattern
    , twinkleTwinkleChordsPattern
    )


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