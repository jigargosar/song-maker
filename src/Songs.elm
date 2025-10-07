module Songs exposing (SongConfig, allSongs, parseSong)

import Instruments exposing (PercType)


type alias SongConfig =
    { name : String
    , displayName : String
    , melody : List (List String) -- Each step can have multiple notes
    , percussion : List (List PercType) -- Each step can have multiple drums
    , bpm : Int
    , startingOctave : Int
    , totalOctaves : Int
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


perc1 =
    Instruments.perc1


perc2 =
    Instruments.perc2


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
        [ [ perc1 ], [], [ perc2 ], [] ]
            -- "how I wonder what"
            ++ [ [ perc1 ], [], [ perc2 ], [] ]
            -- "what you are so"
            ++ [ [ perc1 ], [], [ perc2 ], [] ]
            -- "far above the world"
            ++ [ [ perc1 ], [], [ perc2 ], [] ]
            -- "Up above the world"
            ++ [ [ perc1 ], [], [ perc2 ], [] ]
            -- "so high like a"
            ++ [ [ perc1 ], [], [ perc2 ], [] ]
            -- "diamond in the"
            ++ [ [ perc1 ], [], [ perc2 ], [] ]
            -- "sky (end)"
            ++ [ [ perc1 ], [], [ perc2 ], [] ]
    , bpm = 90
    , startingOctave = 3
    , totalOctaves = 3
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
        [ [ perc1 ], [], [ perc2 ], [], [ perc1 ], [], [], [] ]
            ++ [ [ perc1 ], [], [ perc2 ], [], [ perc1 ], [], [], [] ]
            ++ [ [ perc1 ], [], [ perc2 ], [], [ perc1 ], [], [ perc2 ], [] ]
            ++ [ [ perc1 ], [], [ perc2 ], [], [ perc1 ], [], [], [] ]
    , bpm = 120
    , startingOctave = 4
    , totalOctaves = 2
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
        [ [ perc1 ], [], [ perc2 ], [], [ perc1 ], [], [ perc2 ], [] ]
            ++ [ [ perc1 ], [], [ perc2 ], [], [ perc1 ], [], [ perc2 ], [] ]
            ++ [ [ perc1 ], [], [ perc2 ], [], [ perc1 ], [], [ perc2 ], [] ]
            ++ [ [ perc1 ], [], [ perc2 ], [], [ perc1 ], [], [], [] ]
    , bpm = 120
    , startingOctave = 4
    , totalOctaves = 1
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
        [ [ perc1 ], [], [ perc2 ], [], [ perc1 ], [], [ perc2 ], [] ]
            ++ [ [ perc1 ], [], [ perc2 ], [], [ perc1 ], [], [ perc2 ], [] ]
            ++ [ [ perc1 ], [], [ perc2 ], [], [ perc1 ], [], [ perc2 ], [] ]
            ++ [ [ perc1 ], [], [ perc2 ], [], [ perc1 ], [], [ perc2 ], [] ]
    , bpm = 100
    , startingOctave = 3
    , totalOctaves = 2
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
        [ [ perc2 ], [], [ perc1 ], [], [ perc2 ], [], [ perc1 ], [] ]
            ++ [ [ perc2 ], [], [ perc1 ], [], [ perc2 ], [], [ perc1 ], [] ]
            ++ [ [ perc2 ], [], [ perc1 ], [], [ perc2 ], [], [ perc1 ], [] ]
            ++ [ [ perc2 ], [], [ perc1 ], [], [ perc2 ], [], [ perc1 ], [] ]
    , bpm = 120
    , startingOctave = 4
    , totalOctaves = 1
    , bars = 4
    , beatsPerBar = 4
    , subdivisions = 2
    }
