module Instruments exposing
    ( DrumConfig
    , DrumKit
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
    = Accent
    | Bass


type alias DrumConfig =
    { accentMidi : Int
    , accentWebAudioFont : String
    , bassMidi : Int
    , bassWebAudioFont : String
    }


drumKitConfig : DrumKit -> DrumConfig
drumKitConfig kit =
    case kit of
        ElectronicKit ->
            { accentMidi = 38
            , accentWebAudioFont = "_drum_38_0_SBLive_sf2"
            , bassMidi = 36
            , bassWebAudioFont = "_drum_36_0_SBLive_sf2"
            }

        BlockKit ->
            { accentMidi = 38
            , accentWebAudioFont = "_drum_38_0_SBLive_sf2"
            , bassMidi = 36
            , bassWebAudioFont = "_drum_36_0_SBLive_sf2"
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
    [ Accent, Bass ]


percLabel : DrumKit -> PercType -> String
percLabel kit percType =
    case kit of
        ElectronicKit ->
            case percType of
                Accent ->
                    "Snare"

                Bass ->
                    "Kick"

        BlockKit ->
            case percType of
                Accent ->
                    "Click"

                Bass ->
                    "Thud"


percRowIdx : PercType -> Int
percRowIdx percType =
    case percType of
        Accent ->
            0

        Bass ->
            1


defaultDrumKit : DrumKit
defaultDrumKit =
    ElectronicKit


perc1 : PercType
perc1 =
    Bass


perc2 : PercType
perc2 =
    Accent
