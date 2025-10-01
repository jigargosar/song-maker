module Songs exposing (SongConfig, allSongs, parseSong, twinkleSong)

import Instruments exposing (PercType)
import Scales exposing (RootNote, ScaleType)



-- SongConfig type alias


type alias SongConfig =
    { melody : List (List String) -- Each step can have multiple notes
    , percussion : List (List PercType) -- Each step can have multiple drums
    , scaleType : ScaleType
    , rootNote : RootNote
    , bpm : Int
    , octaveStart : Int
    , octaveCount : Int
    , bars : Int
    , beatsPerBar : Int
    , subdivisions : Int
    }


kick =
    Instruments.percKick


snare =
    Instruments.percSnare



-- twinkleSong value


twinkleSong : SongConfig
twinkleSong =
    { melody =
        -- "Twinkle twinkle little star"
        [ [ "C4", "C3", "E4" ], [ "C4" ], [ "G4", "G3", "B4" ], [ "G4" ] ]
            -- "how I wonder what"
            ++ [ [ "A4", "F3", "C5" ], [ "A4" ], [ "G4", "G3" ], [] ]
            -- "what you are so"
            ++ [ [ "F4", "F3", "A4" ], [ "F4" ], [ "E4", "C3", "G4" ], [ "E4" ] ]
            -- "far above the world"
            ++ [ [ "D4", "G3", "B4" ], [ "D4" ], [ "C4", "C3", "E4" ], [] ]
            -- "Up above the world"
            ++ [ [ "G4", "G3", "B4" ], [ "G4" ], [ "F4", "F3", "A4" ], [ "F4" ] ]
            -- "so high like a"
            ++ [ [ "E4", "C3", "G4" ], [ "E4" ], [ "D4", "G3", "B4" ], [ "D4" ] ]
            -- "diamond in the"
            ++ [ [ "G4", "C3", "E4" ], [ "F4" ], [ "E4", "C3", "G4" ], [ "D4" ] ]
            -- "sky (end)"
            ++ [ [ "C4", "C3", "E4" ], [], [], [] ]
    , percussion =
        -- "Twinkle twinkle little star"
        [ [ kick ], [], [ snare ], [] ]
            -- "how I wonder what"
            ++ [ [ kick ], [], [ snare ], [] ]
            -- "what you are so"
            ++ [ [ kick ], [], [ snare ], [] ]
            -- "far above the world"
            ++ [ [ kick ], [], [ snare ], [] ]
            -- "Up above the world"
            ++ [ [ kick ], [], [ snare ], [] ]
            -- "so high like a"
            ++ [ [ kick ], [], [ snare ], [] ]
            -- "diamond in the"
            ++ [ [ kick ], [], [ snare ], [] ]
            -- "sky (end)"
            ++ [ [ kick ], [], [ snare ], [] ]
    , scaleType = Scales.Major
    , rootNote = Scales.C
    , bpm = 90
    , octaveStart = 3
    , octaveCount = 3
    , bars = 4
    , beatsPerBar = 4
    , subdivisions = 2
    }


furEliseSong : SongConfig
furEliseSong =
    { melody =
        -- RH: E D# E D# E B D C A - C E A B - E G# B C - E -
        -- LH: - - - - - - - - - A E A - E E G# - A E A -
        [ [ "E5" ], [ "D#5" ], [ "E5" ], [ "D#5" ], [ "E5" ], [ "B4" ], [ "D5" ], [ "C5" ] ]
            ++ [ [ "A4", "A3" ], [], [ "C5", "E3" ], [ "E5", "A3" ], [ "A5" ], [ "B5", "E3" ], [], [ "E5", "E3" ] ]
            ++ [ [ "G#5", "G#3" ], [ "B5" ], [ "C6", "A3" ], [], [ "E5", "E3" ], [], [ "A3" ], [] ]
            -- RH: E D# E D# E B D C A - C E A B - E C B A
            -- LH: - - - - - - - - - A E A - E E G# - A E A
            ++ [ [ "E5" ], [ "D#5" ], [ "E5" ], [ "D#5" ], [ "E5" ], [ "B4" ], [ "D5" ], [ "C5" ] ]
            ++ [ [ "A4", "A3" ], [], [ "C5", "E3" ], [ "E5", "A3" ], [ "A5" ], [ "B5", "E3" ], [], [ "E5", "E3" ] ]
            ++ [ [ "C6", "G#3" ], [ "B5" ], [ "A5", "A3" ], [], [ "E3" ], [], [ "A3" ], [] ]
    , percussion =
        [ [ kick ], [], [], [], [ snare ], [], [], [] ]
            ++ [ [], [], [], [], [ snare ], [], [], [] ]
            ++ [ [ kick ], [], [], [], [], [], [], [] ]
            ++ [ [ kick ], [], [], [], [ snare ], [], [], [] ]
            ++ [ [], [], [], [], [ snare ], [], [], [] ]
            ++ [ [ kick ], [], [], [], [], [], [], [] ]
    , scaleType = Scales.Chromatic
    , rootNote = Scales.A
    , bpm = 90
    , octaveStart = 3
    , octaveCount = 4
    , bars = 6
    , beatsPerBar = 4
    , subdivisions = 2
    }



-- Song list and parsing


{-| TODO: Add these songs:

  - Happy Birthday
  - Mary Had a Little Lamb
  - Ode to Joy
  - Jingle Bells
  - Für Elise
  - Old MacDonald

List of all available songs for UI display

-}
allSongs : List { name : String, displayName : String }
allSongs =
    [ { name = "twinkle", displayName = "Twinkle Twinkle Little Star" }
    , { name = "fur-elise", displayName = "Für Elise" }
    ]


{-| Parse song name to SongConfig
-}
parseSong : String -> Maybe SongConfig
parseSong name =
    case name of
        "twinkle" ->
            Just twinkleSong

        "fur-elise" ->
            Just furEliseSong

        _ ->
            Nothing
