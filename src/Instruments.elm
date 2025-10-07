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
    | AcousticKit
    | CongaKit


allDrumKits : List DrumKit
allDrumKits =
    [ ElectronicKit, BlockKit, AcousticKit, CongaKit ]


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
            { accentMidi = 76
            , accentWebAudioFont = "_drum_76_0_SBLive_sf2"
            , bassMidi = 77
            , bassWebAudioFont = "_drum_77_0_SBLive_sf2"
            }

        AcousticKit ->
            { accentMidi = 40
            , accentWebAudioFont = "_drum_40_0_SBLive_sf2"
            , bassMidi = 35
            , bassWebAudioFont = "_drum_35_0_SBLive_sf2"
            }

        CongaKit ->
            { accentMidi = 63
            , accentWebAudioFont = "_drum_63_0_SBLive_sf2"
            , bassMidi = 64
            , bassWebAudioFont = "_drum_64_0_SBLive_sf2"
            }


parseDrumKit : String -> DrumKit
parseDrumKit str =
    case str of
        "Electronic" ->
            ElectronicKit

        "Blocks" ->
            BlockKit

        "Kit" ->
            AcousticKit

        "Conga" ->
            CongaKit

        _ ->
            ElectronicKit


drumKitLabel : DrumKit -> String
drumKitLabel drumKit =
    case drumKit of
        ElectronicKit ->
            "Electronic"

        BlockKit ->
            "Blocks"

        AcousticKit ->
            "Kit"

        CongaKit ->
            "Conga"


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

        AcousticKit ->
            case percType of
                Accent ->
                    "Snare"

                Bass ->
                    "Kick"

        CongaKit ->
            case percType of
                Accent ->
                    "High"

                Bass ->
                    "Low"


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


{- NEXT REFACTORING PLAN (NOT YET IMPLEMENTED)

GOAL: Convert DrumKit from custom type to data-driven type alias approach

CURRENT STATE:
- DrumKit is custom type: ElectronicKit | BlockKit | AcousticKit | CongaKit
- Functions use case expressions (lines 124-153, 156-172, 175-188, 196-229)
- Adding new kit requires changes in 5 locations

PLANNED STATE:
- type alias DrumKit = String (internal IDs: "electronic", "blocks", "kit", "conga")
- type alias DrumKitData = { id, label, config, accentLabel, bassLabel } (NOT exported)
- allDrumKitsData : List DrumKitData (internal only, single source of truth)
- Public API unchanged: allDrumKits : List DrumKit (returns List.map .id allDrumKitsData)
- Functions become lookups with defaultDrumKit fallback (no hardcoded values)

ENCAPSULATION PRINCIPLE:
- Module provides API as if DrumKit were opaque type
- Clients treat DrumKit as opaque (no field access, no pattern matching on IDs)
- Don't export DrumKitData - keep it internal

FILES TO UPDATE:
- src/Instruments.elm (this file): Refactor custom type to data-driven
- src/Main.elm: Should work unchanged (uses allDrumKits, drumKitLabel, etc.)
- src/Model.elm: Should work unchanged (uses drumKitConfig, percLabel, etc.)

BENEFIT: Adding new kit = one entry in allDrumKitsData list
-}


{- TODO: Extract duplicate NoteToPlay creation code (NOT YET IMPLEMENTED)

CURRENT STATE:
- Model.elm has duplicate NoteToPlay creation pattern across ~6 functions
- 3 pitch functions and 3 perc functions repeat similar record construction
- Changes to NoteToPlay structure require updates in multiple places

PLAN:
- Extract helper functions for creating NoteToPlay records
- Reduce duplication and centralize NoteToPlay creation logic
- Need to read Model.elm to identify exact duplication patterns

FILES TO AUDIT:
- src/Model.elm: Locate NoteToPlay creation patterns
-}


{- TODO: Audit perc1/perc2 usage (NOT YET IMPLEMENTED)

CURRENT STATE:
- perc1 and perc2 are legacy positional names (lines 247-254)
- Now aliased to semantic Bass and Accent
- Codebase may still use perc1/perc2 directly instead of Bass/Accent

PLAN:
- Search codebase for perc1/perc2 usage
- Replace with semantic Bass/Accent where appropriate
- Keep perc1/perc2 exports only if external consumers depend on them

FILES TO AUDIT:
- src/Main.elm
- src/Model.elm
- Any other files that import from Instruments
-}
