module Instruments exposing
    ( DrumKit
    , PercType
    , TonalInstrument
    , allDrumKits
    , allPercTypes
    , allTonal
    , defaultDrumKit
    , defaultTonalInstrument
    , drumKitConfig
    , drumKitLabel
    , parseDrumKit
    , parseTonal
    , perc1
    , perc2
    , percLabel
    , percRowIdx
    , tonalLabel
      -- Constructors
    , tonalWebAudioFont
    )


type TonalInstrument
    = GrandPianoSBLive
    | MarimbaSBLLive
    | StringsSBLive
    | FluteSBLive


type DrumKit
    = ElectronicKit
    | RockKit


type PercType
    = Perc1
    | Perc2


tonalWebAudioFont : TonalInstrument -> String
tonalWebAudioFont instrument =
    case instrument of
        GrandPianoSBLive ->
            "_tone_0000_SBLive_sf2"

        MarimbaSBLLive ->
            "_tone_0120_SBLive_sf2"

        StringsSBLive ->
            --"_tone_0400_SBLive_sf2"
            "_tone_0450_SBLive_sf2"

        FluteSBLive ->
            --"_tone_0730_SBLive_sf2"
            "_tone_0730_Aspirin_sf2_file"


drumKitConfig : DrumKit -> { kickWebAudioFont : String, kickMidi : Int, snareWebAudioFont : String, snareMidi : Int }
drumKitConfig kit =
    case kit of
        ElectronicKit ->
            { kickWebAudioFont = "_drum_36_0_SBLive_sf2"
            , kickMidi = 36
            , snareWebAudioFont = "_drum_38_0_SBLive_sf2"
            , snareMidi = 38
            }

        RockKit ->
            { kickWebAudioFont = "_drum_36_0_SBLive_sf2"
            , kickMidi = 36
            , snareWebAudioFont = "_drum_38_0_SBLive_sf2"
            , snareMidi = 38
            }


allTonal : List TonalInstrument
allTonal =
    [ GrandPianoSBLive, MarimbaSBLLive, StringsSBLive, FluteSBLive ]


allDrumKits : List DrumKit
allDrumKits =
    [ ElectronicKit, RockKit ]


tonalLabel : TonalInstrument -> String
tonalLabel instrument =
    case instrument of
        GrandPianoSBLive ->
            "Piano"

        MarimbaSBLLive ->
            "Marimba"

        StringsSBLive ->
            "Strings"

        FluteSBLive ->
            "Flute"


drumKitLabel : DrumKit -> String
drumKitLabel drumKit =
    case drumKit of
        ElectronicKit ->
            "Electronic"

        RockKit ->
            "Rock"


parseTonal : String -> TonalInstrument
parseTonal str =
    case str of
        "Piano" ->
            GrandPianoSBLive

        "Marimba" ->
            MarimbaSBLLive

        "Strings" ->
            StringsSBLive

        "Flute" ->
            FluteSBLive

        _ ->
            defaultTonalInstrument


parseDrumKit : String -> DrumKit
parseDrumKit str =
    case str of
        "Electronic" ->
            ElectronicKit

        "Rock" ->
            RockKit

        _ ->
            ElectronicKit


allPercTypes : List PercType
allPercTypes =
    [ Perc1, Perc2 ]


percLabel : PercType -> String
percLabel percType =
    case percType of
        Perc1 ->
            "Kick"

        Perc2 ->
            "Snare"


percRowIdx : PercType -> Int
percRowIdx percType =
    case percType of
        Perc2 ->
            0

        Perc1 ->
            1



-- Constructor functions


defaultTonalInstrument : TonalInstrument
defaultTonalInstrument =
    GrandPianoSBLive


defaultDrumKit : DrumKit
defaultDrumKit =
    ElectronicKit


perc1 : PercType
perc1 =
    Perc1


perc2 : PercType
perc2 =
    Perc2
