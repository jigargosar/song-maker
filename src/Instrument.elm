module Instrument exposing
    ( PitchInstrument(..)
    , DrumKit(..)
    , PercType(..)
    , pitchInstrumentName
    , drumKitConfig
    , allPitchInstruments
    , allDrumKits
    , pitchInstrumentToString
    , drumKitToString
    , parsePitchInstrument
    , parseDrumKit
    )

{-| This module encapsulates all types and functions related to pitch instruments and drum kits.
-}

type PitchInstrument
    = Piano
    | Marimba

type DrumKit
    = StandardKit
    | RockKit

type PercType
    = Kick
    | Snare

pitchInstrumentName : PitchInstrument -> String
pitchInstrumentName instrument =
    case instrument of
        Piano ->
            "_tone_0250_SoundBlasterOld_sf2"
        Marimba ->
            "_tone_0110_SBLive_sf2"

drumKitConfig : DrumKit -> { kickInstrument : String, kickMidi : Int, snareInstrument : String, snareMidi : Int }
drumKitConfig kit =
    case kit of
        StandardKit ->
            { kickInstrument = "_drum_36_0_SBLive_sf2"
            , kickMidi = 36
            , snareInstrument = "_drum_38_0_SBLive_sf2"
            , snareMidi = 38
            }
        RockKit ->
            { kickInstrument = "_drum_36_0_SBLive_sf2"
            , kickMidi = 36
            , snareInstrument = "_drum_38_0_SBLive_sf2"
            , snareMidi = 38
            }

allPitchInstruments : List PitchInstrument
allPitchInstruments =
    [ Piano, Marimba ]

allDrumKits : List DrumKit
allDrumKits =
    [ StandardKit, RockKit ]

pitchInstrumentToString : PitchInstrument -> String
pitchInstrumentToString instrument =
    case instrument of
        Piano -> "Piano"
        Marimba -> "Marimba"

drumKitToString : DrumKit -> String
drumKitToString drumKit =
    case drumKit of
        StandardKit -> "Standard"
        RockKit -> "Rock"

parsePitchInstrument : String -> PitchInstrument
parsePitchInstrument str =
    case str of
        "Piano" -> Piano
        "Marimba" -> Marimba
        _ -> Piano

parseDrumKit : String -> DrumKit
parseDrumKit str =
    case str of
        "Standard" -> StandardKit
        "Rock" -> RockKit
        _ -> StandardKit
