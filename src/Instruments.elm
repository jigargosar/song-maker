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


type DrumKit
    = ElectronicKit
    | BlockKit


allDrumKits : List DrumKit
allDrumKits =
    [ ElectronicKit, BlockKit ]


type PercType
    = Perc1
    | Perc2


drumKitConfig : DrumKit -> { kickWebAudioFont : String, kickMidi : Int, snareWebAudioFont : String, snareMidi : Int }
drumKitConfig kit =
    case kit of
        ElectronicKit ->
            { kickWebAudioFont = "_drum_36_0_SBLive_sf2"
            , kickMidi = 36
            , snareWebAudioFont = "_drum_38_0_SBLive_sf2"
            , snareMidi = 38
            }

        BlockKit ->
            { kickWebAudioFont = "_drum_36_0_SBLive_sf2"
            , kickMidi = 36
            , snareWebAudioFont = "_drum_38_0_SBLive_sf2"
            , snareMidi = 38
            }


parseDrumKit : String -> DrumKit
parseDrumKit str =
    case str of
        "Electronic" ->
            ElectronicKit

        "Blocks" ->
            BlockKit

        _ ->
            ElectronicKit


drumKitLabel : DrumKit -> String
drumKitLabel drumKit =
    case drumKit of
        ElectronicKit ->
            "Electronic"

        BlockKit ->
            "Blocks"


allPercTypes : List PercType
allPercTypes =
    [ Perc1, Perc2 ]


percLabel : DrumKit -> PercType -> String
percLabel kit percType =
    case kit of
        ElectronicKit ->
            case percType of
                Perc1 ->
                    "Kick"

                Perc2 ->
                    "Snare"

        BlockKit ->
            case percType of
                Perc1 ->
                    "Low"

                Perc2 ->
                    "High"


percRowIdx : PercType -> Int
percRowIdx percType =
    case percType of
        Perc2 ->
            0

        Perc1 ->
            1


allTonal : List TonalInstrument
allTonal =
    [ GrandPianoSBLive, MarimbaSBLLive, StringsSBLive, FluteSBLive ]


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
