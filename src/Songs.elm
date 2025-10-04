module Songs exposing (SongConfig, allSongs, parseSong, twinkleSong)

import Instruments exposing (PercType)
import Scales exposing (RootNote, ScaleType)


type alias SongConfig =
    { name : String
    , displayName : String
    , melody : List (List String) -- Each step can have multiple notes
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
    , scaleType = Scales.Major
    , rootNote = Scales.C
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
    , scaleType = Scales.Major
    , rootNote = Scales.C
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
    , scaleType = Scales.Major
    , rootNote = Scales.C
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
    , scaleType = Scales.Major
    , rootNote = Scales.D
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
    , scaleType = Scales.Major
    , rootNote = Scales.C
    , bpm = 120
    , octaveStart = 4
    , octaveCount = 1
    , bars = 4
    , beatsPerBar = 4
    , subdivisions = 2
    }



{-
URL: http://localhost:1234/?bpm=100&octaveStart=3&octaveCount=2&pitchGrid=4%2C25%2C5%2C26%2C6%2C14%2C6%2C24%2C6%2C27%2C7%2C0%2C7%2C1%2C7%2C4%2C7%2C8%2C7%2C9%2C7%2C12%2C7%2C16%2C7%2C17%2C7%2C23%2C7%2C28%2C7%2C30%2C8%2C2%2C8%2C6%2C8%2C10%2C8%2C18%2C8%2C22%2C9%2C3%2C9%2C5%2C9%2C11%2C9%2C19%2C9%2C21%2C10%2C20&percGrid=&scale=Major&root=C&bars=4&beatsPerBar=4&subdivisions=2&instrument=Piano&drumKit=Electronic

Settings:
- Scale: C Major
- Root: C
- Octave Start: 3 (2 octaves)
- Bars: 4, Beats: 4, Subdivisions: 2 (32 total steps)

Pitch Index to Note mapping (C Major scale):
- 0-6: C3, D3, E3, F3, G3, A3, B3
- 7-13: C4, D4, E4, F4, G4, A4, B4

Notes by step:
Bar 1 (steps 0-7):   C4 C4 D4 E4 | C4 E4 D4 --
Bar 2 (steps 8-15):  C4 C4 D4 E4 | C4 -- B3 --
Bar 3 (steps 16-23): C4 C4 D4 E4 | F4 E4 D4 C4
Bar 4 (steps 24-31): B3 G3 A3 B3 | C4 -- C4 --

Melodic sequence:
C4-C4-D4-E4-C4-E4-D4 | C4-C4-D4-E4-C4-B3 | C4-C4-D4-E4-F4-E4-D4-C4 | B3-G3-A3-B3-C4-C4

This appears to be a variation or exercise using the C Major scale, moving primarily around C4 (middle C) with some descending motion to G3 at the end.
-}