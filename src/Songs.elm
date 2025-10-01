module Songs exposing (SongConfig, allSongs, parseSong, twinkleSong)

import Instruments exposing (PercType)



-- SongConfig type alias


type alias SongConfig =
    { melody : List (List String) -- Each step can have multiple notes
    , percussion : List (List PercType) -- Each step can have multiple drums
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
    , bpm = 90
    , octaveStart = 3
    , octaveCount = 3
    , bars = 4
    , beatsPerBar = 4
    , subdivisions = 2
    }



-- Song list and parsing


{-| List of all available songs for UI display
-}
allSongs : List { name : String, displayName : String }
allSongs =
    [ { name = "twinkle", displayName = "Twinkle Twinkle Little Star" }
    ]


{-| Parse song name to SongConfig
-}
parseSong : String -> Maybe SongConfig
parseSong name =
    case name of
        "twinkle" ->
            Just twinkleSong

        _ ->
            Nothing
