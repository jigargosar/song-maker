module Songs exposing (SongConfig, allSongs, parseSong)

import Instruments exposing (PercType)


type alias SongConfig =
    { name : String
    , displayName : String
    , melody : List (List String) -- Each step can have multiple notes
    , percussion : List (List PercType) -- Each step can have multiple drums
    , bpm : Int
    , octaveStart : Int
    , octaveCount : Int
    , bars : Int
    , beatsPerBar : Int
    , subdivisions : Int
    }


allSongs : List SongConfig
allSongs =
    [ twinkleSong
    , happyBirthdaySong
    , maryHadALittleLambSong
    , odeToJoySong
    , jingleBellsSong
    ]


parseSong : String -> Maybe SongConfig
parseSong name =
    List.filter (\s -> s.name == name) allSongs
        |> List.head


kick =
    Instruments.percKick


snare =
    Instruments.percSnare


twinkleSong : SongConfig
twinkleSong =
    { name = "twinkle"
    , displayName = "Twinkle Twinkle Little Star"
    , melody =
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


happyBirthdaySong : SongConfig
happyBirthdaySong =
    { name = "happy-birthday"
    , displayName = "Happy Birthday"
    , melody =
        -- Happy birthday to you
        [ [ "C4" ], [ "C4" ], [ "D4" ], [ "C4" ], [ "F4" ], [ "E4" ], [], [] ]
            -- Happy birthday to you
            ++ [ [ "C4" ], [ "C4" ], [ "D4" ], [ "C4" ], [ "G4" ], [ "F4" ], [], [] ]
            -- Happy birthday dear...
            ++ [ [ "C4" ], [ "C4" ], [ "C5" ], [ "A4" ], [ "F4" ], [ "E4" ], [ "D4" ], [] ]
            -- Happy birthday to you
            ++ [ [ "A#4" ], [ "A#4" ], [ "A4" ], [ "F4" ], [ "G4" ], [ "F4" ], [], [] ]
    , percussion =
        [ [ kick ], [], [ snare ], [], [ kick ], [], [], [] ]
            ++ [ [ kick ], [], [ snare ], [], [ kick ], [], [], [] ]
            ++ [ [ kick ], [], [ snare ], [], [ kick ], [], [ snare ], [] ]
            ++ [ [ kick ], [], [ snare ], [], [ kick ], [], [], [] ]
    , bpm = 120
    , octaveStart = 4
    , octaveCount = 2
    , bars = 4
    , beatsPerBar = 4
    , subdivisions = 2
    }


maryHadALittleLambSong : SongConfig
maryHadALittleLambSong =
    { name = "mary-lamb"
    , displayName = "Mary Had a Little Lamb"
    , melody =
        -- Mary had a little lamb
        [ [ "E4" ], [ "D4" ], [ "C4" ], [ "D4" ], [ "E4" ], [ "E4" ], [ "E4" ], [] ]
            -- Little lamb, little lamb
            ++ [ [ "D4" ], [ "D4" ], [ "D4" ], [], [ "E4" ], [ "G4" ], [ "G4" ], [] ]
            -- Mary had a little lamb
            ++ [ [ "E4" ], [ "D4" ], [ "C4" ], [ "D4" ], [ "E4" ], [ "E4" ], [ "E4" ], [ "E4" ] ]
            -- Its fleece was white as snow
            ++ [ [ "D4" ], [ "D4" ], [ "E4" ], [ "D4" ], [ "C4" ], [], [], [] ]
    , percussion =
        [ [ kick ], [], [ snare ], [], [ kick ], [], [ snare ], [] ]
            ++ [ [ kick ], [], [ snare ], [], [ kick ], [], [ snare ], [] ]
            ++ [ [ kick ], [], [ snare ], [], [ kick ], [], [ snare ], [] ]
            ++ [ [ kick ], [], [ snare ], [], [ kick ], [], [], [] ]
    , bpm = 120
    , octaveStart = 4
    , octaveCount = 1
    , bars = 4
    , beatsPerBar = 4
    , subdivisions = 2
    }


odeToJoySong : SongConfig
odeToJoySong =
    { name = "ode-to-joy"
    , displayName = "Ode to Joy"
    , melody =
        -- First phrase
        [ [ "C4" ], [ "C4" ], [ "D4" ], [ "E4" ], [ "C4" ], [ "E4" ], [ "D4" ], [] ]
            ++ [ [ "C4" ], [ "C4" ], [ "D4" ], [ "E4" ], [ "C4" ], [], [ "B3" ], [] ]
            -- Second phrase
            ++ [ [ "C4" ], [ "C4" ], [ "D4" ], [ "E4" ], [ "F4" ], [ "E4" ], [ "D4" ], [ "C4" ] ]
            ++ [ [ "B3" ], [ "G3" ], [ "A3" ], [ "B3" ], [ "C4" ], [], [ "C4" ], [] ]
    , percussion =
        [ [ kick ], [], [ snare ], [], [ kick ], [], [ snare ], [] ]
            ++ [ [ kick ], [], [ snare ], [], [ kick ], [], [ snare ], [] ]
            ++ [ [ kick ], [], [ snare ], [], [ kick ], [], [ snare ], [] ]
            ++ [ [ kick ], [], [ snare ], [], [ kick ], [], [ snare ], [] ]
    , bpm = 100
    , octaveStart = 3
    , octaveCount = 2
    , bars = 4
    , beatsPerBar = 4
    , subdivisions = 2
    }


jingleBellsSong : SongConfig
jingleBellsSong =
    { name = "jingle-bells"
    , displayName = "Jingle Bells"
    , melody =
        [ [ "E4" ], [ "E4" ], [ "E4" ], [], [ "E4" ], [ "E4" ], [ "E4" ], [] ]
            ++ [ [ "E4" ], [ "G4" ], [ "C4" ], [ "D4" ], [ "E4" ], [], [], [] ]
            ++ [ [ "F4" ], [ "F4" ], [ "F4" ], [ "F4" ], [ "F4" ], [ "E4" ], [ "E4" ], [ "E4" ] ]
            ++ [ [ "E4" ], [ "D4" ], [ "D4" ], [ "E4" ], [ "D4" ], [], [ "G4" ], [] ]
    , percussion =
        [ [ snare ], [], [ kick ], [], [ snare ], [], [ kick ], [] ]
            ++ [ [ snare ], [], [ kick ], [], [ snare ], [], [ kick ], [] ]
            ++ [ [ snare ], [], [ kick ], [], [ snare ], [], [ kick ], [] ]
            ++ [ [ snare ], [], [ kick ], [], [ snare ], [], [ kick ], [] ]
    , bpm = 120
    , octaveStart = 4
    , octaveCount = 1
    , bars = 4
    , beatsPerBar = 4
    , subdivisions = 2
    }
