# Song Maker - Architecture Design v2

**Date:** 2025-10-09
**Status:** DESIGN - Ready for Implementation

---

## Module Overview

| Module                    | Purpose                        | Imports                                                                                                                 |
|---------------------------|--------------------------------|-------------------------------------------------------------------------------------------------------------------------|
| **Grid**                  | Generic (Int,Int) grid storage | Set                                                                                                                     |
| **History**               | Generic undo/redo              | -                                                                                                                       |
| **Drawing**               | Drawing state machine          | -                                                                                                                       |
| **Utils**                 | Generic helpers                | Set, Basics.Extra                                                                                                       |
| **Scales**                | Musical scale theory           | List.Extra                                                                                                              |
| **Timing**                | Time/BPM calculations          | -                                                                                                                       |
| **TonalInstruments**      | Tonal instrument config        | List.Extra                                                                                                              |
| **PercussionInstruments** | Drum kit config + PercType     | List.Extra                                                                                                              |
| **Sequencer**             | Playback timing state          | -                                                                                                                       |
| **Session**               | Editing state coordinator      | Grid, Scales, Timing, TonalInstruments, PercussionInstruments, Url.*                                                    |
| **Songs**                 | Preset song data               | PercussionInstruments                                                                                                   |
| **Main**                  | Application orchestrator       | Session, History, Sequencer, Drawing, Scales, TonalInstruments, PercussionInstruments, Songs, Utils, Browser, Html, Url |

---

## Module Specifications

### Grid

**Internal Type:**
```elm
type Grid = Grid (Set (Int, Int))
```

**Critical API:**
```elm
empty : Grid
set : (Int, Int) -> Bool -> Grid -> Grid
get : (Int, Int) -> Grid -> Bool
toggle : (Int, Int) -> Grid -> Grid
map : ((Int, Int) -> (Int, Int)) -> Grid -> Grid
filter : ((Int, Int) -> Bool) -> Grid -> Grid
toList : Grid -> List (Int, Int)
serialize : Grid -> String
deserialize : String -> Grid
```

**Notes:**
- Opaque type
- Specific to (Int, Int), NOT generic
- Zero dependencies

---

### History

**Internal Type:**
```elm
type History state = History
    { current : state
    , past : List state
    , future : List state
    }
```

**Critical API:**
```elm
init : state -> History state
current : History state -> state
push : state -> History state -> History state
undo : History state -> History state
redo : History state -> History state
canUndo : History state -> Bool
canRedo : History state -> Bool
```

**Notes:**
- Generic over any state type
- Zero dependencies

---

### Drawing

**Internal Type:**
```elm
type Drawing
    = Idle
    | DrawingPitch
    | ErasingPitch
    | DrawingPerc
    | ErasingPerc
```

**Critical API:**
```elm
idle : Drawing
startPitch : Bool -> Drawing -> Drawing
continuePitch : Drawing -> Maybe Bool
startPerc : Bool -> Drawing -> Drawing
continuePerc : Drawing -> Maybe Bool
stop : Drawing -> Drawing
```

**Notes:**
- State machine for drawing interactions
- Zero dependencies

---

### Sequencer

**Internal Type:**
```elm
type Sequencer
    = Stopped
    | Playing { startTime : Float, nextStep : Int, totalSteps : Int, stepDuration : Float }
```

**Critical API:**
```elm
init : Sequencer
play : { totalSteps : Int, stepDuration : Float, currentTime : Float } -> Sequencer -> Sequencer
playFromStep : Int -> { totalSteps : Int, stepDuration : Float, currentTime : Float } -> Sequencer -> Sequencer
stop : Sequencer -> Sequencer
tick : Float -> Sequencer -> ( Sequencer, List Int )
currentStep : Sequencer -> Maybe Int
```

**Notes:**
- Returns step indices to play, no domain knowledge
- Zero dependencies

---

### Scales

**Internal Types:**
```elm
type ScaleType = Major | Pentatonic | Chromatic
type RootNote = C | CSharp | D | DSharp | E | F | FSharp | G | GSharp | A | ASharp | B

type alias ScaleConfig =
    { scaleType : ScaleType
    , rootNote : RootNote
    , startingOctave : Int
    , totalOctaves : Int
    }
```

**Critical API:**
```elm
pitchIdxToMidi : Int -> ScaleConfig -> Int
pitchIdxToNoteName : Int -> ScaleConfig -> String
getTotalPitches : ScaleConfig -> Int
getRootNoteOffset : RootNote -> Int
validateMidi : Int -> ScaleConfig -> Maybe Int
scaleLabel : ScaleType -> String
parseScaleType : String -> ScaleType
rootNoteToString : RootNote -> String
parseRootNote : String -> RootNote
```

**Notes:**
- Pure musical theory
- Zero dependencies (except List.Extra)

---

### Timing

**Internal Types:**
```elm
type alias TimeConfig =
    { bars : Int
    , beatsPerBar : Int
    , subdivisions : Int
    , bpm : Int
    }
```

**Critical API:**
```elm
getTotalSteps : TimeConfig -> Int
noteDuration : TimeConfig -> Float
validateStep : Int -> TimeConfig -> Maybe Int
```

**Notes:**
- Pure time calculations
- Zero dependencies

---

### TonalInstruments

**Internal Types:**
```elm
type TonalInstrument = TonalInstrument String  -- opaque, String internally

type alias TonalInstrumentData =  -- NOT exported
    { id : String
    , label : String
    , webAudioFont : String
    }

allTonalData : List TonalInstrumentData  -- Single source of truth
```

**Critical API:**
```elm
allTonal : List TonalInstrument
defaultTonal : TonalInstrument
tonalLabel : TonalInstrument -> String
parseTonal : String -> TonalInstrument
webAudioFont : TonalInstrument -> String
```

**Notes:**
- Data-driven (add instrument = one entry in allTonalData)
- Opaque type (clients can't construct directly)
- Zero domain dependencies

---

### PercussionInstruments

**Internal Types:**
```elm
type DrumKit = DrumKit String  -- opaque, String internally
type PercType = Accent | Bass  -- custom type (only 2 values)

type alias DrumKitData =  -- NOT exported
    { id : String
    , label : String
    , accentMidi : Int
    , accentWebAudioFont : String
    , accentLabel : String
    , bassMidi : Int
    , bassWebAudioFont : String
    , bassLabel : String
    }

allDrumKitData : List DrumKitData  -- Single source of truth

type alias DrumConfig =
    { accentMidi : Int
    , accentWebAudioFont : String
    , bassMidi : Int
    , bassWebAudioFont : String
    }
```

**Critical API:**
```elm
allDrumKits : List DrumKit
allPercTypes : List PercType
defaultDrumKit : DrumKit
drumKitLabel : DrumKit -> String
parseDrumKit : String -> DrumKit
drumKitConfig : DrumKit -> DrumConfig
percLabel : DrumKit -> PercType -> String
percRowIdx : PercType -> Int  -- PercType → Int for grid storage
```

**Notes:**
- Data-driven (add drumkit = one entry in allDrumKitData)
- percRowIdx: Accent → 0, Bass → 1
- Zero domain dependencies

---

### Session

**Internal Types:**
```elm
type Session = Session
    { pitchGrid : Grid
    , percGrid : Grid
    , scaleType : ScaleType
    , rootNote : RootNote
    , startingOctave : Int
    , totalOctaves : Int
    , bars : Int
    , beatsPerBar : Int
    , subdivisions : Int
    , bpm : Int
    , tonalInstrument : TonalInstrument
    , drumKit : DrumKit
    }

type alias ScaleConfig = Scales.ScaleConfig
type alias TimeConfig = Timing.TimeConfig

type alias InstrumentConfig =
    { tonal : TonalInstrument
    , drumKit : DrumKit
    }

type alias PitchPosition =
    { pitchIdx : Int, stepIdx : Int }

type alias PercPosition =
    { percType : PercType, stepIdx : Int }

type alias NoteToPlay =
    { webAudioFont : String
    , midi : Int
    , duration : Float
    , volume : Float
    }
```

**Critical API:**

*Initialization:*
```elm
init : Session
```

*Config Queries (grouped):*
```elm
scaleConfig : Session -> ScaleConfig
timeConfig : Session -> TimeConfig
instrumentConfig : Session -> InstrumentConfig
bpm : Session -> Int
totalSteps : Session -> Int
totalPitches : Session -> Int
```

*Grid Queries:*
```elm
isPitchActive : PitchPosition -> Session -> Bool
isPercActive : PercPosition -> Session -> Bool
getNotesForStep : Int -> Session -> List NoteToPlay
```

*Display Queries:*
```elm
pitchIdxToNoteName : Int -> Session -> String
percTypeLabel : PercType -> Session -> String
```

*Grid Updates:*
```elm
togglePitch : PitchPosition -> Session -> Session
togglePerc : PercPosition -> Session -> Session
setPitch : PitchPosition -> Bool -> Session -> Session
setPerc : PercPosition -> Bool -> Session -> Session
```

*Config Updates:*
```elm
setScaleType : ScaleType -> Session -> Session
setRootNote : RootNote -> Session -> Session
setBPM : Int -> Session -> Session
setBars : Int -> Session -> Session
setBeatsPerBar : Int -> Session -> Session
setSubdivisions : Int -> Session -> Session
setOctaveRange : Int -> Int -> Session -> Session
setTonalInstrument : TonalInstrument -> Session -> Session
setDrumKit : DrumKit -> Session -> Session
```

*Transformations:*
```elm
resizePitchGrid : Session -> Session
transposePitchGrid : Session -> Session
shiftStepRight : Int -> Session -> Session
deleteStep : Int -> Session -> Session
```

*Serialization:*
```elm
toQueryString : Session -> String
fromUrl : Url -> Session
toQueryStringIfChanged : Url -> Session -> Maybe String
```

**Notes:**
- Opaque type
- Owns NoteToPlay type (constructs inline, no factory functions)
- Owns serialization (no separate Codec module)
- Coordinates all domain modules
- Uses config groups instead of individual field getters
- Handles both pitchGrid and percGrid (not split into separate modules)

---

### Songs

**Internal Types:**
```elm
type alias SongConfig =
    { name : String
    , displayName : String
    , melody : List (List String)
    , percussion : List (List PercType)
    , bpm : Int
    , startingOctave : Int
    , totalOctaves : Int
    , bars : Int
    , beatsPerBar : Int
    , subdivisions : Int
    }
```

**Critical API:**
```elm
allSongs : List SongConfig
parseSong : String -> Maybe SongConfig
```

**Notes:**
- Preset song data
- Session.loadSong will consume SongConfig

---

### Main

**Internal Types:**
```elm
type alias Model =
    { history : History Session
    , sequencer : Sequencer
    , drawing : Drawing
    , audioContextTime : Float
    , url : Url
    , key : Nav.Key
    }

type Msg
    = StartDrawingPitch PitchPosition
    | ContinueDrawingPitch PitchPosition
    | StopDrawing
    | StartDrawingPerc PercPosition
    | ContinueDrawingPerc PercPosition
    | Play
    | Stop
    | TimeSync Float
    | ChangeScaleType ScaleType
    | ChangeRootNote RootNote
    | ChangeBPM Int
    | ChangeTonalInstrument TonalInstrument
    | ChangeDrumKit DrumKit
    | ChangeBars Int
    | ChangeBeatsPerBar Int
    | ChangeSubdivisions Int
    | Undo
    | Redo
    | Reset
    | LoadSong String
    | Save
    | PlayFromStep Int
    | ShiftStepRight Int
    | DeleteStep Int
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
```

**Critical API:**
```elm
init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
update : Msg -> Model -> ( Model, Cmd Msg )
view : Model -> Browser.Document Msg
subscriptions : Model -> Sub Msg
```

**Notes:**
- Orchestrates all modules
- Delegates to Session/History/Sequencer/Drawing
- Thin layer - no business logic

---

## Key Design Principles

1. **Opaque Types**: Grid, Session, History, Drawing, Sequencer all opaque
2. **Zero Dependencies**: 8 out of 12 modules have zero domain dependencies
3. **Data-Driven**: TonalInstruments and PercussionInstruments use data lists instead of case expressions
4. **Session Owns Serialization**: No separate Codec module
5. **Config Groups**: Session exposes scaleConfig/timeConfig/instrumentConfig instead of individual getters
6. **Direct Construction**: Session creates NoteToPlay inline, no factory functions
7. **Grid Simplicity**: Grid is (Int, Int) specific, not generic

---

## Module Dependency Summary

**Zero Dependencies (8 modules):**
- Grid
- History
- Drawing
- Utils
- Scales
- Timing
- TonalInstruments
- PercussionInstruments
- Sequencer

**Domain Coordinator (1 module):**
- Session → Grid, Scales, Timing, TonalInstruments, PercussionInstruments, Url.*

**Application (2 modules):**
- Songs → PercussionInstruments
- Main → Session, History, Sequencer, Drawing, Scales, TonalInstruments, PercussionInstruments, Songs, Utils, Browser, Html, Url

---

## Implementation Notes

### Adding New Instrument
**TonalInstrument:** Add one entry to `allTonalData` list in TonalInstruments.elm
**DrumKit:** Add one entry to `allDrumKitData` list in PercussionInstruments.elm

### Adding New Session Field
1. Add to Session type
2. Add getter/setter (or add to existing config group)
3. Add to serialization (toQueryString, fromUrl) if needed

### Grid Operations
- Common operations (shift, delete): Session coordinates both grids
- Pitch-specific (resize, transpose): Session handles with domain knowledge
- Perc resize: simpler (only validates stepIdx, not percRowIdx)

---

## Open Questions

###  View Model
- **Decision needed:** Where should ViewModel conversion live?
- **Options:**
  - Stay in Session (as query functions)
  - Move to Main (view-specific)
  - Separate View module
- **Current:** Model.elm has `toVm` function

###  Song loading
- **Decision needed:** What should Songs module return?
- **Options:**
  - Return SongConfig (current approach)
  - Return Session directly
  - Return builder/function
- **Impact:** How Session.loadSong is implemented

