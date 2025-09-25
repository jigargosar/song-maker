module Instruments exposing
    ( DrumKit(..)
    , PercType(..)
    , TonalInstrument(..)
    , allDrumKits
    , allPercTypes
    , allTonal
    , drumKitConfig
    , drumKitLabel
    , parseDrumKit
    , parseTonal
    , percLabel
    , percRowIdx
    , tonalJsVarName
    , tonalLabel
    )


type TonalInstrument
    = GrandPianoSBLive
    | MarimbaSBLLive


type DrumKit
    = StandardKit
    | RockKit


type PercType
    = Kick
    | Snare


tonalJsVarName : TonalInstrument -> String
tonalJsVarName instrument =
    case instrument of
        GrandPianoSBLive ->
            "_tone_0000_SBLive_sf2"

        MarimbaSBLLive ->
            "_tone_0120_SBLive_sf2"


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


allTonal : List TonalInstrument
allTonal =
    [ GrandPianoSBLive, MarimbaSBLLive ]


allDrumKits : List DrumKit
allDrumKits =
    [ StandardKit, RockKit ]


tonalLabel : TonalInstrument -> String
tonalLabel instrument =
    case instrument of
        GrandPianoSBLive ->
            "Piano"

        MarimbaSBLLive ->
            "Marimba"


drumKitLabel : DrumKit -> String
drumKitLabel drumKit =
    case drumKit of
        StandardKit ->
            "Standard"

        RockKit ->
            "Rock"


parseTonal : String -> TonalInstrument
parseTonal str =
    case str of
        "Piano" ->
            GrandPianoSBLive

        "Marimba" ->
            MarimbaSBLLive

        _ ->
            GrandPianoSBLive


parseDrumKit : String -> DrumKit
parseDrumKit str =
    case str of
        "Standard" ->
            StandardKit

        "Rock" ->
            RockKit

        _ ->
            StandardKit


allPercTypes : List PercType
allPercTypes =
    [ Kick, Snare ]


percLabel : PercType -> String
percLabel percType =
    case percType of
        Kick ->
            "Kick"

        Snare ->
            "Snare"


percRowIdx : PercType -> Int
percRowIdx percType =
    case percType of
        Snare ->
            0

        Kick ->
            1
