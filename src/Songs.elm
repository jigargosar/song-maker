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


stairwayToHeavenSong : SongConfig
stairwayToHeavenSong =
    { melody =
        -- Intro arpeggio pattern
        -- Measure 1 (Am): A C E A C E C A with bass A
        [ [ "A4", "A3" ], [ "C5" ], [ "E5" ], [ "A5" ], [ "C5" ], [ "E5" ], [ "C5" ], [ "A4" ] ]
            -- Measure 2 (Am/G#): Same pattern with bass G#
            ++ [ [ "A4", "G#3" ], [ "C5" ], [ "E5" ], [ "A5" ], [ "C5" ], [ "E5" ], [ "C5" ], [ "A4" ] ]
            -- Measure 3 (Am/G): Same pattern with bass G
            ++ [ [ "A4", "G3" ], [ "C5" ], [ "E5" ], [ "A5" ], [ "C5" ], [ "E5" ], [ "C5" ], [ "A4" ] ]
            -- Measure 4 (D/F#): D F# A D F# A F# D with bass F#
            ++ [ [ "D5", "F#3" ], [ "F#4" ], [ "A4" ], [ "D5" ], [ "F#4" ], [ "A4" ], [ "F#4" ], [ "D4" ] ]
            -- Measure 5 (Fmaj7): F A C F A C A F with bass F
            ++ [ [ "F4", "F3" ], [ "A4" ], [ "C5" ], [ "F5" ], [ "A4" ], [ "C5" ], [ "A4" ], [ "F4" ] ]
            -- Measure 6 (G - Am): G B D G B D G D with bass pattern
            ++ [ [ "G4" ], [ "B4" ], [ "D5" ], [ "G5" ], [ "B4" ], [ "D5" ], [ "G4" ], [ "D4" ] ]
    , percussion =
        [ [], [], [], [], [], [], [], [] ]
            ++ [ [], [], [], [], [], [], [], [] ]
            ++ [ [], [], [], [], [], [], [], [] ]
            ++ [ [], [], [], [], [], [], [], [] ]
            ++ [ [], [], [], [], [], [], [], [] ]
            ++ [ [], [], [], [], [], [], [], [] ]
    , scaleType = Scales.Chromatic
    , rootNote = Scales.A
    , bpm = 66
    , octaveStart = 3
    , octaveCount = 3
    , bars = 6
    , beatsPerBar = 4
    , subdivisions = 2
    }


happyBirthdaySong : SongConfig
happyBirthdaySong =
    { melody =
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
    , octaveStart = 3
    , octaveCount = 3
    , bars = 4
    , beatsPerBar = 4
    , subdivisions = 2
    }


maryHadALittleLambSong : SongConfig
maryHadALittleLambSong =
    { melody =
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
    , octaveStart = 3
    , octaveCount = 3
    , bars = 4
    , beatsPerBar = 4
    , subdivisions = 2
    }


odeToJoySong : SongConfig
odeToJoySong =
    { melody =
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
    { melody =
        -- Jingle bells, jingle bells, jingle all the way
        [ [ "E4" ], [ "E4" ], [ "E4" ], [], [ "E4" ], [ "E4" ], [ "E4" ], [] ]
            ++ [ [ "E4" ], [ "G4" ], [ "C4" ], [], [ "D4" ], [], [ "E4" ], [] ]
            -- Oh what fun it is to ride in a one-horse open sleigh
            ++ [ [ "F4" ], [ "F4" ], [ "F4" ], [ "F4" ], [ "F4" ], [ "E4" ], [ "E4" ], [ "E4" ] ]
            ++ [ [ "E4" ], [ "D4" ], [ "D4" ], [ "E4" ], [ "D4" ], [], [ "G4" ], [] ]
    , percussion =
        [ [ kick ], [], [ snare ], [], [ kick ], [], [ snare ], [] ]
            ++ [ [ kick ], [], [ snare ], [], [ kick ], [], [ snare ], [] ]
            ++ [ [ kick ], [], [ snare ], [], [ kick ], [], [ snare ], [] ]
            ++ [ [ kick ], [], [ snare ], [], [ kick ], [], [ snare ], [] ]
    , scaleType = Scales.Major
    , rootNote = Scales.C
    , bpm = 140
    , octaveStart = 3
    , octaveCount = 3
    , bars = 4
    , beatsPerBar = 4
    , subdivisions = 2
    }


oldMacDonaldSong : SongConfig
oldMacDonaldSong =
    { melody =
        -- Old MacDonald had a farm: C4 C4 C4 G4 A4 A4 G4
        [ [ "C4" ], [ "C4" ], [ "C4" ], [ "G4" ], [ "A4" ], [ "A4" ], [ "G4" ] ]
            -- E-I-E-I-O: E4 E4 D4 D4 C4 _ _ _
            ++ [ [ "E4" ], [ "E4" ], [ "D4" ], [ "D4" ], [ "C4" ], [], [], [] ]
            -- And on that farm he had a cow: G4 G4 F4 F4 E4 E4 D4
            ++ [ [ "G4" ], [ "G4" ], [ "F4" ], [ "F4" ], [ "E4" ], [ "E4" ], [ "D4" ] ]
            -- E-I-E-I-O: _ _ G4 G4 F4 F4 E4 E4 D4
            ++ [ [], [], [ "G4" ], [ "G4" ], [ "F4" ], [ "F4" ], [ "E4" ], [ "E4" ], [ "D4" ] ]
            -- With a moo moo here: C4 _ G4 G4 C4 _ _ _
            ++ [ [ "C4" ], [], [ "G4" ], [ "G4" ], [ "C4" ], [], [], [] ]
            -- And a moo moo there: C4 _ G4 G4 C4 _ _ _
            ++ [ [ "C4" ], [], [ "G4" ], [ "G4" ], [ "C4" ], [], [], [] ]
            -- Here a moo, there a moo: G4 G4 G4 G4 G4 G4 _
            ++ [ [ "G4" ], [ "G4" ], [ "G4" ], [ "G4" ], [ "G4" ], [ "G4" ], [] ]
            -- Everywhere a moo moo: C4 _ G4 G4 C4 _ _ _
            ++ [ [ "C4" ], [], [ "G4" ], [ "G4" ], [ "C4" ], [], [], [] ]
            -- Old MacDonald had a farm: C4 C4 C4 G4 A4 A4 G4
            ++ [ [ "C4" ], [ "C4" ], [ "C4" ], [ "G4" ], [ "A4" ], [ "A4" ], [ "G4" ] ]
            -- E-I-E-I-O: E4 E4 D4 D4 C4
            ++ [ [ "E4" ], [ "E4" ], [ "D4" ], [ "D4" ], [ "C4" ] ]
    , percussion =
        [ [ kick ], [], [ snare ], [], [ kick ], [], [ snare ] ]
            ++ [ [ kick ], [], [ snare ], [], [ kick ], [], [], [] ]
            ++ [ [ kick ], [], [ snare ], [], [ kick ], [], [ snare ] ]
            ++ [ [], [], [ kick ], [], [ snare ], [], [ kick ], [], [ snare ] ]
            ++ [ [ kick ], [], [ snare ], [], [ kick ], [], [], [] ]
            ++ [ [ kick ], [], [ snare ], [], [ kick ], [], [], [] ]
            ++ [ [ kick ], [], [ snare ], [], [ kick ], [], [] ]
            ++ [ [ kick ], [], [ snare ], [], [ kick ], [], [], [] ]
            ++ [ [ kick ], [], [ snare ], [], [ kick ], [], [ snare ] ]
            ++ [ [ kick ], [], [ snare ], [], [ kick ] ]
    , scaleType = Scales.Major
    , rootNote = Scales.C
    , bpm = 160
    , octaveStart = 3
    , octaveCount = 3
    , bars = 71
    , beatsPerBar = 1
    , subdivisions = 1
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
    , { name = "stairway", displayName = "Stairway to Heaven (Intro)" }
    , { name = "happy-birthday", displayName = "Happy Birthday" }
    , { name = "mary-lamb", displayName = "Mary Had a Little Lamb" }
    , { name = "ode-to-joy", displayName = "Ode to Joy" }
    , { name = "jingle-bells", displayName = "Jingle Bells" }
    , { name = "old-macdonald", displayName = "Old MacDonald" }
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

        "stairway" ->
            Just stairwayToHeavenSong

        "happy-birthday" ->
            Just happyBirthdaySong

        "mary-lamb" ->
            Just maryHadALittleLambSong

        "ode-to-joy" ->
            Just odeToJoySong

        "jingle-bells" ->
            Just jingleBellsSong

        "old-macdonald" ->
            Just oldMacDonaldSong

        _ ->
            Nothing
