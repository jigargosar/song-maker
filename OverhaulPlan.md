# Song Maker Architecture Remodeling Plan

**Date:** 2025-10-09
**Status:** DRAFT - Under Review
**Goal:** Redesign module architecture to achieve better encapsulation, minimal cross-module dependencies, and reduce multi-location updates when adding features.

---

## TABLE OF CONTENTS

1. [Current Architecture Analysis](#current-architecture-analysis)
2. [Critical Problems Identified](#critical-problems-identified)
3. [Design Principles](#design-principles)
4. [New Module Structure](#new-module-structure)
5. [Migration Impact Analysis](#migration-impact-analysis)
6. [Implementation Plan](#implementation-plan)
7. [Open Questions](#open-questions)

---

## CURRENT ARCHITECTURE ANALYSIS

### Module Dependency Graph

```
Grid → Instruments, Scales, Timing
Model → Grid, Scales, Instruments, Timing, Songs, QuerystringCodec
QuerystringCodec → Grid, Instruments, Scales
Main → Grid, Model, Scales, Instruments
Songs → Instruments
Timing → (no dependencies)
Scales → (no dependencies)
Instruments → (no dependencies)
Utils → (no dependencies)
```

### Module Sizes (lines of code)

- **Model.elm**: 878 lines ⚠️ (God module)
- **Main.elm**: 848 lines (mostly view code)
- **Grid.elm**: 290 lines
- **Instruments.elm**: 301 lines
- **QuerystringCodec.elm**: 157 lines
- **Scales.elm**: 404 lines
- **Songs.elm**: 191 lines
- **Timing.elm**: 45 lines
- **Utils.elm**: 72 lines

### Current Responsibilities by Module

**Model.elm** (TOO MANY RESPONSIBILITIES):
- Grid state management
- Scale configuration
- Timing configuration
- Drawing state machine
- Playback state machine
- History/undo/redo system
- Audio note scheduling
- URL serialization helpers
- Song loading
- View model conversion

**Grid.elm**:
- Grid data structure (Set-based)
- Position types
- Grid operations (set/get cells)
- Grid transformations (resize, transpose, shift, delete)
- Serialization/deserialization
- **Problem**: Depends on Instruments, Scales, Timing (wrong direction)

**Instruments.elm**:
- TonalInstrument custom type
- DrumKit custom type
- PercType custom type
- Label/parsing functions (5+ case expressions per type)
- WebAudioFont name lookups

**Scales.elm**:
- Scale types and root notes
- MIDI conversion logic
- Note name conversion
- Scale configuration

**Timing.elm**:
- Time signature configuration
- Step count calculation
- Note duration calculation

---

## CRITICAL PROBLEMS IDENTIFIED

### 1. Model.elm is a God Module (878 lines)

**Responsibilities:** Drawing, playback, history, audio, serialization, conversions

**Exposes:** 24+ functions

**Maintenance Burden:** Lines 122-149 contain a massive comment documenting **11 places** to update when adding a single field to Model:

```elm
{-| CRITICAL: When adding ANY field to Model, decide: "Should this be tracked in history and URL?"

If YES → Update all 11 locations:
1. Model type - ADD HERE FIRST
2. HistoryState type
3. QueryParams type - as Maybe Type
4. toHistoryState function
5. updateModelFromHistoryState function
6. applyQueryDefaults function
7. applyQueryParams function
8. queryParser function
9. buildQuery function
10. applySong function - if applicable
11. SongConfig type in Songs.elm - if songs should specify this field
-}
```

**Code Duplication:** NoteToPlay record creation duplicated in 6+ locations across Model.elm

**Dependencies:** Depends on everything: Grid, Scales, Instruments, Timing, QuerystringCodec, Songs

---

### 2. Tight Coupling & Circular Dependencies

**Problem:** Modules reference each other in multiple directions, making changes ripple across codebase.

**Example - Grid depends on domain modules:**
```elm
-- Grid.elm line 106
percPositionToTuple : PercPos -> ( PercRowIdx, StepIdx )
percPositionToTuple { percType, stepIdx } =
    ( Instruments.percRowIdx percType, stepIdx )  -- Grid calling Instruments!
```

This is backwards - Grid (infrastructure) shouldn't know about Instruments (domain).

---

### 3. Leaky Abstractions

**Grid Module exposes internal implementation:**
```elm
-- EXPOSED in module definition:
type alias PitchGrid = Set PitchCell  -- Clients see it's a Set!
type alias PitchCell = ( MidiNote, StepIdx )  -- Clients see it's a tuple!

-- This allows clients to do:
Set.empty  -- instead of Grid.initialPitchGrid
Set.member -- instead of Grid.isPitchCellActive
```

**Raw tuples everywhere:**
- `(MidiNote, StepIdx)` exposed as PitchCell
- `(PercRowIdx, StepIdx)` exposed in PercGrid
- Type safety lost - could accidentally swap row/step indices

**Instruments exposes constructors:**
- Module comment (lines 258-285) acknowledges DrumKit should be data-driven but isn't yet
- Adding instrument requires updating 5 case expressions

---

### 4. Multiple Update Points for Single Changes

**Adding a new instrument (DrumKit):**
- 5 locations to update across Instruments.elm:
  1. Add constructor to `DrumKit` type (line 99-104)
  2. Update `drumKitConfig` case expression (line 124-153)
  3. Update `parseDrumKit` case expression (line 156-172)
  4. Update `drumKitLabel` case expression (line 175-188)
  5. Update `percLabel` nested case expression (line 196-229)

**Adding a new field to Model:**
- **11 locations** to update (see comment in Model.elm:122-149)
- HistoryState duplicates Model structure
- QueryParams duplicates Model structure
- Triple maintenance: Model, HistoryState, QueryParams

**Adding a new TonalInstrument:**
- 4 locations: type definition, label function, parser function, webAudioFont function
- All use case expressions that must be kept in sync

---

### 5. HistoryState Mirrors Model

**Current approach:**
```elm
type alias Model =
    { pitchGrid : PitchGrid
    , percGrid : PercGrid
    , scaleType : ScaleType
    , rootNote : RootNote
    , startingOctave : Int
    , totalOctaves : Int
    -- ... 12 more fields ...
    , undoStack : List HistoryState  -- Contains most of the same fields!
    , redoStack : List HistoryState
    }

type alias HistoryState =  -- Duplicates Model fields
    { pitchGrid : PitchGrid
    , percGrid : PercGrid
    , scaleType : ScaleType
    -- ... duplicated fields ...
    }
```

**Problem:** Adding a field requires updating both types plus conversion functions.

---

### 6. No Clear Module Boundaries

**Questions that are hard to answer:**
- Where does "editing" end and "playback" begin?
- Who owns the concept of "a note to play"?
- Is drawing a UI concern or a domain concern?
- Should Grid know about MIDI notes, or just positions?

**Result:** Logic spreads across modules with unclear ownership.

---

## DESIGN PRINCIPLES

The remodeling follows these principles:

### 1. Information Hiding
- Modules expose minimal opaque types
- Internal representation is hidden (no `type alias = Set`)
- Clients cannot access implementation details

### 2. Single Responsibility
- Each module owns ONE concept
- Clear boundaries between modules
- No god modules

### 3. Data-Driven Design
- Replace case expressions with data lookups
- Single source of truth for configuration
- Adding entries doesn't require code changes

### 4. Make Impossible States Impossible
- Use types to prevent invalid states
- Custom types for domain concepts
- Opaque types for encapsulation

### 5. Dependency Direction
- High-level modules depend on low-level modules
- Domain modules don't depend on infrastructure
- Infrastructure is generic and reusable

### 6. Minimize Cross-Module Updates
- Adding a field should update 1-3 places, not 11
- Related code should be co-located
- Duplication is a code smell

---

## NEW MODULE STRUCTURE

### Overview

**New module hierarchy:**
```
Infrastructure Layer (generic, reusable):
├── Grid (generic grid for any comparable position)
├── History (generic undo/redo for any state)
└── Utils (helpers)

Domain Layer (music/sequencer specific):
├── Scales (note/scale theory)
├── Instruments (instrument configuration)
├── Timing (time signature/BPM calculations)
├── Session (editing state: grids + configs)
├── Sequencer (playback state machine)
└── Drawing (drawing interaction state)

Integration Layer:
├── Codec (URL serialization)
├── Songs (preset songs)
└── Main (orchestrates everything)
```

**Key changes:**
- Model.elm → Split into Session, Sequencer, Drawing
- Grid.elm → Made generic (no domain dependencies)
- Instruments.elm → Data-driven (no case expressions for kits)
- New History module → Generic undo/redo
- New Codec module → Simplified serialization

---

### 1. Grid Module (REDESIGNED)

**File:** `src/Grid.elm`
**Responsibilities:** Generic grid storage for any comparable position
**Size:** ~50 lines (down from 290)

**Current Problems:**
- Exposes `Set` implementation: `type alias PitchGrid = Set PitchCell`
- Depends on domain modules: Instruments (for `percRowIdx`), Scales (for MIDI), Timing (for validation)
- Position types leak implementation: `type alias PitchCell = (MidiNote, StepIdx)`
- Clients can use `Set.empty` instead of proper API

**New Design:**

```elm
module Grid exposing
    ( Grid
    , empty
    , set
    , toggle
    , get
    , toList
    , map
    , filter
    , serialize
    , deserialize
    )

-- OPAQUE - implementation hidden
type Grid comparable
    = Grid (Set comparable)

-- Pure grid operations - no domain knowledge
empty : Grid comparable
empty = Grid Set.empty

set : comparable -> Bool -> Grid comparable -> Grid comparable
set position isActive (Grid s) =
    Grid (if isActive then Set.insert position s else Set.remove position s)

toggle : comparable -> Grid comparable -> Grid comparable
toggle position ((Grid s) as grid) =
    set position (not (get position grid)) grid

get : comparable -> Grid comparable -> Bool
get position (Grid s) =
    Set.member position s

toList : Grid comparable -> List comparable
toList (Grid s) =
    Set.toList s

map : (a -> comparable) -> Grid a -> Grid comparable
map fn (Grid s) =
    Grid (Set.map fn s)

filter : (comparable -> Bool) -> Grid comparable -> Grid comparable
filter predicate (Grid s) =
    Grid (Set.filter predicate s)

-- Serialization for (Int, Int) grids
serialize : Grid (Int, Int) -> String
serialize (Grid s) =
    s
        |> Set.toList
        |> List.concatMap (\(a, b) -> [a, b])
        |> List.map String.fromInt
        |> String.join ","

deserialize : String -> Grid (Int, Int)
deserialize str =
    str
        |> String.split ","
        |> List.filterMap String.toInt
        |> toPairs
        |> Set.fromList
        |> Grid

-- Helper
toPairs : List a -> List (a, a)
toPairs list =
    case list of
        x :: y :: rest -> (x, y) :: toPairs rest
        _ -> []
```

**Benefits:**
- ✅ No dependencies on domain modules
- ✅ Fully opaque - clients can't use `Set.empty`
- ✅ Generic - works for any comparable position type
- ✅ ~50 lines instead of 290
- ✅ Independently testable
- ✅ Reusable in other projects

**Migration notes:**
- PitchGrid becomes `Grid (Int, Int)` where first Int is MIDI note, second is step
- PercGrid becomes `Grid (Int, Int)` where first Int is perc row (0=accent, 1=bass), second is step
- Domain logic (MIDI conversion, perc type mapping) moves to Session module

---

### 2. History Module (NEW)

**File:** `src/History.elm`
**Responsibilities:** Generic undo/redo for any state type
**Size:** ~40 lines

**Current Problems:**
- No separate History module exists
- HistoryState type in Model.elm duplicates Model fields (lines 106-119)
- toHistoryState/updateModelFromHistoryState functions are boilerplate (lines 201-237)
- Adding a Model field requires updating 4 history-related locations
- Undo/redo logic embedded in Model.elm (lines 551-583)

**New Design:**

```elm
module History exposing
    ( History
    , init
    , current
    , push
    , undo
    , redo
    , canUndo
    , canRedo
    )

-- OPAQUE - generic over any state type
type History state
    = History
        { current : state
        , past : List state
        , future : List state
        }

init : state -> History state
init initialState =
    History
        { current = initialState
        , past = []
        , future = []
        }

current : History state -> state
current (History h) =
    h.current

push : state -> History state -> History state
push newState (History h) =
    if newState == h.current then
        History h  -- Don't add duplicate
    else
        History
            { current = newState
            , past = h.current :: h.past
            , future = []  -- Clear redo stack
            }

undo : History state -> History state
undo (History h) =
    case h.past of
        [] ->
            History h

        previousState :: olderStates ->
            History
                { current = previousState
                , past = olderStates
                , future = h.current :: h.future
                }

redo : History state -> History state
redo (History h) =
    case h.future of
        [] ->
            History h

        nextState :: laterStates ->
            History
                { current = nextState
                , past = h.current :: h.past
                , future = laterStates
                }

canUndo : History state -> Bool
canUndo (History h) =
    not (List.isEmpty h.past)

canRedo : History state -> Bool
canRedo (History h) =
    not (List.isEmpty h.future)
```

**Benefits:**
- ✅ Generic - works for ANY state type (Session, Model, anything)
- ✅ ~40 lines of focused code
- ✅ No domain knowledge required
- ✅ Adding Session field: **0 changes to History module**
- ✅ Independently testable
- ✅ Reusable in other projects

**Usage in Main:**
```elm
type alias Model =
    { history : History Session  -- History wraps Session
    , sequencer : Sequencer
    , drawing : Drawing
    , -- ... other fields
    }

-- When user makes a change:
update msg model =
    case msg of
        TogglePitchCell pos ->
            let
                newSession = Session.togglePitch pos (History.current model.history)
            in
            { model | history = History.push newSession model.history }
```

---

### 3. Drawing Module (NEW)

**File:** `src/Drawing.elm`
**Responsibilities:** Track drawing interaction state machine
**Size:** ~30 lines

**Current Problems:**
- DrawState type in Model.elm (lines 98-103) mixed with other concerns
- Drawing logic spread across update functions in Model.elm (lines 586-746)
- No clear state machine boundaries

**New Design:**

```elm
module Drawing exposing
    ( Drawing
    , idle
    , startPitch
    , continuePitch
    , startPerc
    , continuePerc
    , stop
    , Mode(..)
    , mode
    )

-- OPAQUE
type Drawing
    = Idle
    | DrawingPitch
    | ErasingPitch
    | DrawingPerc
    | ErasingPerc

type Mode
    = NotDrawing
    | AddingPitches
    | RemovingPitches
    | AddingPerc
    | RemovingPerc

idle : Drawing
idle = Idle

mode : Drawing -> Mode
mode drawing =
    case drawing of
        Idle -> NotDrawing
        DrawingPitch -> AddingPitches
        ErasingPitch -> RemovingPitches
        DrawingPerc -> AddingPerc
        ErasingPerc -> RemovingPerc

startPitch : Bool -> Drawing -> Drawing
startPitch cellIsActive drawing =
    case drawing of
        Idle ->
            if cellIsActive then ErasingPitch else DrawingPitch
        _ ->
            drawing

continuePitch : Drawing -> Maybe Bool
continuePitch drawing =
    case drawing of
        DrawingPitch -> Just True
        ErasingPitch -> Just False
        _ -> Nothing

startPerc : Bool -> Drawing -> Drawing
startPerc cellIsActive drawing =
    case drawing of
        Idle ->
            if cellIsActive then ErasingPerc else DrawingPerc
        _ ->
            drawing

continuePerc : Drawing -> Maybe Bool
continuePerc drawing =
    case drawing of
        DrawingPerc -> Just True
        ErasingPerc -> Just False
        _ -> Nothing

stop : Drawing -> Drawing
stop _ = Idle
```

**Benefits:**
- ✅ Clear state machine with well-defined transitions
- ✅ ~30 lines
- ✅ No domain knowledge
- ✅ Type-safe state transitions
- ✅ Independently testable

---

### 4. Sequencer Module (NEW)

**File:** `src/Sequencer.elm`
**Responsibilities:** Manage playback state and timing
**Size:** ~80 lines

**Current Problems:**
- PlayState type in Model.elm (lines 92-95) mixed with editing state
- Sequencer logic scattered across Model.elm:
  - `startPlaying` (lines 502-509)
  - `playFromStep` (lines 512-518)
  - `stop` (lines 525-535)
  - `onTimeSync` (lines 749-797)
  - `getCurrentPlayingStep` (lines 327-337)
  - `getActiveNotesForStep` (lines 362-423)

**New Design:**

```elm
module Sequencer exposing
    ( Sequencer
    , State(..)
    , init
    , play
    , stop
    , playFromStep
    , tick
    , currentStep
    , state
    )

-- OPAQUE
type Sequencer
    = Stopped
    | Playing { startTime : Float, nextStep : Int, totalSteps : Int, stepDuration : Float }

type State
    = NotPlaying
    | PlayingMusic

init : Sequencer
init = Stopped

state : Sequencer -> State
state seq =
    case seq of
        Stopped -> NotPlaying
        Playing _ -> PlayingMusic

play : { totalSteps : Int, stepDuration : Float, currentTime : Float } -> Sequencer -> Sequencer
play config sequencer =
    case sequencer of
        Stopped ->
            Playing
                { startTime = config.currentTime
                , nextStep = 1
                , totalSteps = config.totalSteps
                , stepDuration = config.stepDuration
                }
        Playing _ ->
            sequencer

playFromStep : Int -> { totalSteps : Int, stepDuration : Float, currentTime : Float } -> Sequencer -> Sequencer
playFromStep stepIdx config _ =
    let
        adjustedStartTime =
            config.currentTime - (toFloat stepIdx * config.stepDuration)
    in
    Playing
        { startTime = adjustedStartTime
        , nextStep = stepIdx + 1
        , totalSteps = config.totalSteps
        , stepDuration = config.stepDuration
        }

stop : Sequencer -> Sequencer
stop _ = Stopped

tick : Float -> Sequencer -> ( Sequencer, List Int )
tick currentTime sequencer =
    case sequencer of
        Stopped ->
            ( sequencer, [] )

        Playing config ->
            let
                elapsedTime = currentTime - config.startTime
                currentStepFloat = elapsedTime / config.stepDuration
                currentStepInt = floor currentStepFloat
            in
            if currentStepInt >= config.nextStep then
                let
                    stepToPlay = modBy config.totalSteps config.nextStep
                in
                ( Playing { config | nextStep = config.nextStep + 1 }
                , [ stepToPlay ]
                )
            else
                ( sequencer, [] )

currentStep : Sequencer -> Maybe Int
currentStep sequencer =
    case sequencer of
        Stopped ->
            Nothing

        Playing config ->
            Just (modBy config.totalSteps (config.nextStep - 1))
```

**Benefits:**
- ✅ Separates timing logic from state management
- ✅ ~80 lines of focused code
- ✅ No domain knowledge (works with step indices)
- ✅ Returns step indices to play, doesn't know about notes/MIDI
- ✅ Independently testable
- ✅ Clear API

**Usage in Main:**
```elm
update msg model =
    case msg of
        TimeSync audioTime ->
            let
                (newSequencer, stepsToPlay) =
                    Sequencer.tick audioTime model.sequencer

                session = History.current model.history

                notesToPlay =
                    stepsToPlay
                        |> List.concatMap (\stepIdx -> Session.getNotesForStep stepIdx session)
            in
            ( { model | sequencer = newSequencer }
            , Cmd.batch (List.map playNote notesToPlay)
            )
```

---

### 5. Session Module (NEW - REPLACES Model.elm core state)

**File:** `src/Session.elm`
**Responsibilities:** Hold editing state (grids, scale, timing, instruments)
**Size:** ~250 lines (vs Model.elm's 878)

**Current Problems:**
- Model.elm does TOO MUCH (878 lines)
- Editing state mixed with drawing state, playback state, history, URL state
- 24+ exported functions with unclear groupings
- Adding a field requires updating 11 places (see Model.elm:122-149)

**New Design:**

```elm
module Session exposing
    ( Session
    , init, empty

    -- Types
    , ScaleConfig, TimeConfig
    , PitchPosition, PercPosition
    , NoteToPlay

    -- Queries - Configuration
    , scaleConfig, timeConfig
    , tonalInstrument, drumKit, bpm
    , totalSteps, totalPitches

    -- Queries - Grid
    , isPitchActive, isPercActive
    , pitchGridToList, percGridToList

    -- Queries - Display
    , pitchIdxToNoteName
    , percTypeLabel

    -- Queries - Playback
    , getNotesForStep

    -- Updates - Grid
    , togglePitch, togglePerc
    , setPitch, setPerc

    -- Updates - Scale
    , setScaleType, setRootNote
    , setOctaveRange

    -- Updates - Time
    , setBars, setBeatsPerBar, setSubdivisions
    , setBPM

    -- Updates - Instruments
    , setTonalInstrument, setDrumKit

    -- Transformations
    , resizePitchGrid, transposePitchGrid
    , shiftStepRight, deleteStep

    -- Bulk Operations
    , loadSong
    )

import Grid exposing (Grid)
import Instruments exposing (TonalInstrument, DrumKit, PercType)
import Scales exposing (ScaleType, RootNote)

-- OPAQUE
type Session
    = Session
        { pitchGrid : Grid ( Int, Int )  -- (midiNote, stepIdx)
        , percGrid : Grid ( Int, Int )   -- (percRowIdx, stepIdx)
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

-- Config types for external use
type alias ScaleConfig =
    { scaleType : ScaleType
    , rootNote : RootNote
    , startingOctave : Int
    , totalOctaves : Int
    }

type alias TimeConfig =
    { bars : Int
    , beatsPerBar : Int
    , subdivisions : Int
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

-- Initialization
init : Session
init =
    Session
        { pitchGrid = Grid.empty
        , percGrid = Grid.empty
        , scaleType = Scales.Major
        , rootNote = Scales.C
        , startingOctave = 3
        , totalOctaves = 3
        , bars = 8
        , beatsPerBar = 4
        , subdivisions = 2
        , bpm = 120
        , tonalInstrument = Instruments.defaultTonal
        , drumKit = Instruments.defaultDrumKit
        }

empty : Session
empty = init

-- Queries
scaleConfig : Session -> ScaleConfig
scaleConfig (Session s) =
    { scaleType = s.scaleType
    , rootNote = s.rootNote
    , startingOctave = s.startingOctave
    , totalOctaves = s.totalOctaves
    }

timeConfig : Session -> TimeConfig
timeConfig (Session s) =
    { bars = s.bars
    , beatsPerBar = s.beatsPerBar
    , subdivisions = s.subdivisions
    }

tonalInstrument : Session -> TonalInstrument
tonalInstrument (Session s) = s.tonalInstrument

drumKit : Session -> DrumKit
drumKit (Session s) = s.drumKit

bpm : Session -> Int
bpm (Session s) = s.bpm

totalSteps : Session -> Int
totalSteps session =
    let tc = timeConfig session
    in tc.bars * tc.beatsPerBar * tc.subdivisions

totalPitches : Session -> Int
totalPitches session =
    let sc = scaleConfig session
    in Scales.getTotalPitches sc

isPitchActive : PitchPosition -> Session -> Bool
isPitchActive pos (Session s) =
    let
        sc = scaleConfig (Session s)
        midiNote = Scales.pitchIdxToMidi pos.pitchIdx sc
    in
    Grid.get ( midiNote, pos.stepIdx ) s.pitchGrid

isPercActive : PercPosition -> Session -> Bool
isPercActive pos (Session s) =
    let
        percRowIdx = Instruments.percRowIdx pos.percType
    in
    Grid.get ( percRowIdx, pos.stepIdx ) s.percGrid

pitchIdxToNoteName : Int -> Session -> String
pitchIdxToNoteName pitchIdx session =
    Scales.pitchIdxToNoteName pitchIdx (scaleConfig session)

percTypeLabel : PercType -> Session -> String
percTypeLabel percType session =
    Instruments.percLabel (drumKit session) percType

-- Grid Updates
togglePitch : PitchPosition -> Session -> Session
togglePitch pos (Session s) =
    let
        sc = scaleConfig (Session s)
        midiNote = Scales.pitchIdxToMidi pos.pitchIdx sc
    in
    Session { s | pitchGrid = Grid.toggle ( midiNote, pos.stepIdx ) s.pitchGrid }

setPitch : PitchPosition -> Bool -> Session -> Session
setPitch pos isActive (Session s) =
    let
        sc = scaleConfig (Session s)
        midiNote = Scales.pitchIdxToMidi pos.pitchIdx sc
    in
    Session { s | pitchGrid = Grid.set ( midiNote, pos.stepIdx ) isActive s.pitchGrid }

togglePerc : PercPosition -> Session -> Session
togglePerc pos (Session s) =
    let
        percRowIdx = Instruments.percRowIdx pos.percType
    in
    Session { s | percGrid = Grid.toggle ( percRowIdx, pos.stepIdx ) s.percGrid }

setPerc : PercPosition -> Bool -> Session -> Session
setPerc pos isActive (Session s) =
    let
        percRowIdx = Instruments.percRowIdx pos.percType
    in
    Session { s | percGrid = Grid.set ( percRowIdx, pos.stepIdx ) isActive s.percGrid }

-- Configuration Updates
setScaleType : ScaleType -> Session -> Session
setScaleType newScaleType (Session s) =
    let
        newSession = Session { s | scaleType = newScaleType }
        sc = scaleConfig newSession
        tc = timeConfig newSession
    in
    newSession |> resizePitchGridInternal sc tc

setRootNote : RootNote -> Session -> Session
setRootNote newRootNote (Session s) =
    let
        oldRootNote = s.rootNote
        semitonesDelta = Scales.getRootNoteOffset newRootNote - Scales.getRootNoteOffset oldRootNote
        transposedGrid =
            s.pitchGrid
                |> Grid.map (\( midi, step ) -> ( midi + semitonesDelta, step ))
    in
    Session { s | rootNote = newRootNote, pitchGrid = transposedGrid }

setBPM : Int -> Session -> Session
setBPM newBPM (Session s) =
    Session { s | bpm = max 1 newBPM }

-- ... (more update functions)

-- Playback Support
getNotesForStep : Int -> Session -> List NoteToPlay
getNotesForStep stepIdx session =
    let
        pitchNotes = getPitchNotesForStep stepIdx session
        percNotes = getPercNotesForStep stepIdx session
    in
    pitchNotes ++ percNotes

getPitchNotesForStep : Int -> Session -> List NoteToPlay
getPitchNotesForStep stepIdx (Session s) =
    let
        sc = scaleConfig (Session s)
        duration = noteDurationSeconds (Session s)

        isActiveAt pitchIdx =
            isPitchActive { pitchIdx = pitchIdx, stepIdx = stepIdx } (Session s)
    in
    List.range 0 (Scales.getTotalPitches sc - 1)
        |> List.filterMap
            (\pitchIdx ->
                if isActiveAt pitchIdx then
                    Just (Instruments.createPitchNote
                        s.tonalInstrument
                        (Scales.pitchIdxToMidi pitchIdx sc)
                        duration
                        0.5  -- volume
                    )
                else
                    Nothing
            )

getPercNotesForStep : Int -> Session -> List NoteToPlay
getPercNotesForStep stepIdx (Session s) =
    let
        duration = noteDurationSeconds (Session s)
    in
    Instruments.allPercTypes
        |> List.filterMap
            (\percType ->
                if isPercActive { percType = percType, stepIdx = stepIdx } (Session s) then
                    Just (Instruments.createPercNote s.drumKit percType duration 0.7)
                else
                    Nothing
            )

noteDurationSeconds : Session -> Float
noteDurationSeconds session =
    let tc = timeConfig session
        bpmValue = bpm session
    in
    (60.0 / toFloat bpmValue) / toFloat tc.subdivisions
```

**Benefits:**
- ✅ Clear, focused responsibility: editing state only
- ✅ ~250 lines vs Model's 878
- ✅ No drawing/playback/history mixed in
- ✅ Adding field: update Session type + getter/setter only (2-3 places vs 11)
- ✅ Clean API grouped by concern
- ✅ Independently testable
- ✅ All grid domain logic in one place

**Migration notes:**
- Model.elm's editing state moves here
- Model.elm's drawing state → Drawing module
- Model.elm's playback state → Sequencer module
- Model.elm's history → History module wrapping Session
- Model.elm's NoteToPlay creation → Instruments factory functions

---

### 6. Instruments Module (DATA-DRIVEN REFACTOR)

**File:** `src/Instruments.elm`
**Responsibilities:** Instrument configuration and NoteToPlay creation
**Size:** ~150 lines (down from 301)

**Current Problems:**
- Custom types with case expressions everywhere
- Adding new DrumKit requires updating 5 locations (lines 124-153, 156-172, 175-188, 196-229)
- Adding new TonalInstrument requires updating 4 locations
- Module comment (lines 258-285) acknowledges it should be data-driven but isn't
- NoteToPlay creation duplicated 6+ times in Model.elm

**New Design:**

```elm
module Instruments exposing
    ( TonalInstrument  -- opaque (String internally)
    , DrumKit  -- opaque (String internally)
    , PercType(..)  -- remains custom type
    , DrumConfig

    -- Lists
    , allTonal, allDrumKits, allPercTypes

    -- Labels
    , tonalLabel, drumKitLabel, percLabel

    -- Defaults
    , defaultTonal, defaultDrumKit

    -- Parsers
    , parseTonal, parseDrumKit

    -- Config Getters
    , tonalWebAudioFont, drumKitConfig

    -- Row Index
    , percRowIdx

    -- Constructors (NEW - eliminates duplication)
    , createPitchNote, createPercNote

    -- Types
    , NoteToPlay
    )

-- OPAQUE TYPES (String internally, but clients can't see that)
type TonalInstrument = TonalInstrument String
type DrumKit = DrumKit String

-- Custom type (only 2 values, unlikely to grow)
type PercType = Accent | Bass

type alias NoteToPlay =
    { webAudioFont : String
    , midi : Int
    , duration : Float
    , volume : Float
    }

type alias DrumConfig =
    { accentMidi : Int
    , accentWebAudioFont : String
    , bassMidi : Int
    , bassWebAudioFont : String
    }

-- INTERNAL DATA (not exported) - Single Source of Truth
type alias TonalInstrumentData =
    { id : String
    , label : String
    , webAudioFont : String
    }

allTonalData : List TonalInstrumentData
allTonalData =
    [ { id = "piano"
      , label = "Piano"
      , webAudioFont = "_tone_0000_SBLive_sf2"
      }
    , { id = "marimba"
      , label = "Marimba"
      , webAudioFont = "_tone_0120_SBLive_sf2"
      }
    , { id = "strings"
      , label = "Strings"
      , webAudioFont = "_tone_0450_SBLive_sf2"
      }
    , { id = "flute"
      , label = "Flute"
      , webAudioFont = "_tone_0730_Aspirin_sf2_file"
      }
    -- ADDING NEW INSTRUMENT = ADD ONE ENTRY HERE, DONE!
    ]

type alias DrumKitData =
    { id : String
    , label : String
    , accentMidi : Int
    , accentWebAudioFont : String
    , accentLabel : String
    , bassMidi : Int
    , bassWebAudioFont : String
    , bassLabel : String
    }

allDrumKitData : List DrumKitData
allDrumKitData =
    [ { id = "electronic"
      , label = "Electronic"
      , accentMidi = 38
      , accentWebAudioFont = "_drum_38_0_SBLive_sf2"
      , accentLabel = "Snare"
      , bassMidi = 36
      , bassWebAudioFont = "_drum_36_0_SBLive_sf2"
      , bassLabel = "Kick"
      }
    , { id = "blocks"
      , label = "Blocks"
      , accentMidi = 76
      , accentWebAudioFont = "_drum_76_0_SBLive_sf2"
      , accentLabel = "Click"
      , bassMidi = 77
      , bassWebAudioFont = "_drum_77_0_SBLive_sf2"
      , bassLabel = "Thud"
      }
    , { id = "kit"
      , label = "Kit"
      , accentMidi = 40
      , accentWebAudioFont = "_drum_40_0_SBLive_sf2"
      , accentLabel = "Snare"
      , bassMidi = 35
      , bassWebAudioFont = "_drum_35_0_SBLive_sf2"
      , bassLabel = "Kick"
      }
    , { id = "conga"
      , label = "Conga"
      , accentMidi = 63
      , accentWebAudioFont = "_drum_63_0_SBLive_sf2"
      , accentLabel = "High"
      , bassMidi = 64
      , bassWebAudioFont = "_drum_64_0_SBLive_sf2"
      , bassLabel = "Low"
      }
    -- ADDING NEW KIT = ADD ONE ENTRY HERE, DONE!
    ]

-- PUBLIC API (no case expressions, just lookups)

allTonal : List TonalInstrument
allTonal =
    List.map (\d -> TonalInstrument d.id) allTonalData

allDrumKits : List DrumKit
allDrumKits =
    List.map (\d -> DrumKit d.id) allDrumKitData

defaultTonal : TonalInstrument
defaultTonal =
    TonalInstrument "piano"

defaultDrumKit : DrumKit
defaultDrumKit =
    DrumKit "electronic"

tonalLabel : TonalInstrument -> String
tonalLabel (TonalInstrument id) =
    allTonalData
        |> List.find (\d -> d.id == id)
        |> Maybe.map .label
        |> Maybe.withDefault "Piano"

parseTonal : String -> TonalInstrument
parseTonal label =
    allTonalData
        |> List.find (\d -> d.label == label)
        |> Maybe.map (\d -> TonalInstrument d.id)
        |> Maybe.withDefault defaultTonal

tonalWebAudioFont : TonalInstrument -> String
tonalWebAudioFont (TonalInstrument id) =
    allTonalData
        |> List.find (\d -> d.id == id)
        |> Maybe.map .webAudioFont
        |> Maybe.withDefault "_tone_0000_SBLive_sf2"

drumKitLabel : DrumKit -> String
drumKitLabel (DrumKit id) =
    allDrumKitData
        |> List.find (\d -> d.id == id)
        |> Maybe.map .label
        |> Maybe.withDefault "Electronic"

parseDrumKit : String -> DrumKit
parseDrumKit label =
    allDrumKitData
        |> List.find (\d -> d.label == label)
        |> Maybe.map (\d -> DrumKit d.id)
        |> Maybe.withDefault defaultDrumKit

drumKitConfig : DrumKit -> DrumConfig
drumKitConfig (DrumKit id) =
    case List.find (\d -> d.id == id) allDrumKitData of
        Just data ->
            { accentMidi = data.accentMidi
            , accentWebAudioFont = data.accentWebAudioFont
            , bassMidi = data.bassMidi
            , bassWebAudioFont = data.bassWebAudioFont
            }
        Nothing ->
            drumKitConfig defaultDrumKit

percLabel : DrumKit -> PercType -> String
percLabel (DrumKit id) percType =
    case List.find (\d -> d.id == id) allDrumKitData of
        Just data ->
            case percType of
                Accent -> data.accentLabel
                Bass -> data.bassLabel
        Nothing ->
            percLabel defaultDrumKit percType

allPercTypes : List PercType
allPercTypes = [ Accent, Bass ]

percRowIdx : PercType -> Int
percRowIdx percType =
    case percType of
        Accent -> 0
        Bass -> 1

-- FACTORY FUNCTIONS (NEW - eliminates Model.elm duplication)

createPitchNote : TonalInstrument -> Int -> Float -> Float -> NoteToPlay
createPitchNote instrument midiNote duration volume =
    { webAudioFont = tonalWebAudioFont instrument
    , midi = midiNote
    , duration = duration
    , volume = volume
    }

createPercNote : DrumKit -> PercType -> Float -> Float -> NoteToPlay
createPercNote kit percType duration volume =
    let
        config = drumKitConfig kit
        (webAudioFont, midi) =
            case percType of
                Accent -> (config.accentWebAudioFont, config.accentMidi)
                Bass -> (config.bassWebAudioFont, config.bassMidi)
    in
    { webAudioFont = webAudioFont
    , midi = midi
    , duration = duration
    , volume = volume
    }
```

**Benefits:**
- ✅ Adding instrument: **1 line** (one entry in allTonalData or allDrumKitData)
- ✅ No case expressions for instruments (replaced with List.find lookups)
- ✅ Encapsulation maintained (clients treat TonalInstrument/DrumKit as opaque)
- ✅ Centralizes NoteToPlay creation (eliminates 6+ duplications in Model.elm)
- ✅ ~150 lines vs 301 (nearly half the size)
- ✅ Single source of truth: allTonalData, allDrumKitData

**Migration notes:**
- TonalInstrument/DrumKit change from custom types to opaque type aliases
- All instrument config in data lists instead of case expressions
- Model.elm's NoteToPlay creation moves to createPitchNote/createPercNote

---

### 7. Codec Module (SIMPLIFIED)

**File:** `src/Codec.elm` (replaces QuerystringCodec.elm)
**Responsibilities:** URL serialization/deserialization for Session
**Size:** ~100 lines (down from 157)

**Current Problems:**
- Works with generic `QueryData a` extensible record
- Duplicates Session structure in QueryParams type (lines 35-48)
- Adding a field requires updating 6 places (QueryParams + encode + decode + defaults + apply + songConfig)
- Name is too specific: "QuerystringCodec" vs generic "Codec"

**New Design:**

```elm
module Codec exposing
    ( encode
    , decode
    , reset
    )

import Grid exposing (Grid)
import Instruments
import Scales
import Session exposing (Session)
import Url exposing (Url)
import Url.Builder as UB
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as Query

-- Encode Session to URL query string
encode : Session -> String
encode session =
    let
        sc = Session.scaleConfig session
        tc = Session.timeConfig session
    in
    UB.absolute []
        [ UB.int "bpm" (Session.bpm session)
        , UB.int "startingOctave" sc.startingOctave
        , UB.int "totalOctaves" sc.totalOctaves
        , UB.string "pitchGrid" (Grid.serialize (Session.pitchGridInternal session))
        , UB.string "percGrid" (Grid.serialize (Session.percGridInternal session))
        , UB.string "scale" (Scales.scaleLabel sc.scaleType)
        , UB.string "root" (Scales.rootNoteToString sc.rootNote)
        , UB.int "bars" tc.bars
        , UB.int "beatsPerBar" tc.beatsPerBar
        , UB.int "subdivisions" tc.subdivisions
        , UB.string "instrument" (Instruments.tonalLabel (Session.tonalInstrument session))
        , UB.string "drumKit" (Instruments.drumKitLabel (Session.drumKit session))
        ]

-- Decode URL to Session (or return default Session)
decode : Url -> Session
decode url =
    case url.query of
        Nothing ->
            Session.init

        Just _ ->
            parseUrl url
                |> Maybe.withDefault Session.init

reset : Session
reset =
    Session.init

-- Internal
parseUrl : Url -> Maybe Session
parseUrl url =
    Parser.parse (Parser.top <?> queryParser) url
        |> Maybe.andThen identity
        |> Maybe.map applyToSession

queryParser : Query.Parser (Maybe QueryParams)
queryParser =
    Query.map8 QueryParams
        (Query.int "bpm")
        (Query.int "startingOctave")
        (Query.int "totalOctaves")
        (Query.string "pitchGrid" |> Query.map (Maybe.map Grid.deserialize))
        (Query.string "percGrid" |> Query.map (Maybe.map Grid.deserialize))
        -- ... etc
        |> Just

type alias QueryParams =
    { bpm : Maybe Int
    , startingOctave : Maybe Int
    , totalOctaves : Maybe Int
    , pitchGrid : Maybe (Grid (Int, Int))
    , percGrid : Maybe (Grid (Int, Int))
    -- ... etc
    }

applyToSession : QueryParams -> Session
applyToSession params =
    let
        base = Session.init
    in
    base
        |> applyIfPresent params.bpm Session.setBPM
        |> applyIfPresent params.startingOctave (\v s -> Session.setOctaveRange v (Session.scaleConfig s).totalOctaves s)
        -- ... etc

applyIfPresent : Maybe a -> (a -> Session -> Session) -> Session -> Session
applyIfPresent maybeValue fn session =
    case maybeValue of
        Just value -> fn value session
        Nothing -> session
```

**Benefits:**
- ✅ Works directly with Session type (no generic extensible record)
- ✅ Adding Session field: update 2 places (encode + decode) vs 6
- ✅ Simpler API: encode/decode/reset vs load/serialize/reset/parseQueryParams/buildQueryString
- ✅ ~100 lines vs 157

**Migration notes:**
- Rename QuerystringCodec.elm → Codec.elm
- Remove generic `QueryData a` extensible record support
- Work directly with Session type

---

### 8. Main.elm - The Orchestrator

**File:** `src/Main.elm`
**Responsibilities:** Coordinate modules, handle view, manage Elm Architecture
**Size:** ~600 lines (down from 848)

**Current Problems:**
- Directly manipulates Model which is a god object
- Mixed concerns: view + updates + coordination
- 848 lines (mostly view code, which is OK)

**New Design:**

```elm
port module Main exposing (main)

import Browser
import History exposing (History)
import Session exposing (Session)
import Sequencer exposing (Sequencer)
import Drawing exposing (Drawing)
import Instruments
import Codec
-- ... other imports

type alias Model =
    { history : History Session  -- History wraps Session
    , sequencer : Sequencer
    , drawing : Drawing
    , audioContextTime : Float
    , url : Url
    , key : Nav.Key
    }

init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        initialSession = Codec.decode url
    in
    ( { history = History.init initialSession
      , sequencer = Sequencer.init
      , drawing = Drawing.idle
      , audioContextTime = 0.0
      , url = url
      , key = key
      }
    , Cmd.none
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        session = History.current model.history
    in
    case msg of
        StartDrawingPitch pos ->
            let
                cellIsActive = Session.isPitchActive pos session
                newDrawing = Drawing.startPitch cellIsActive model.drawing
                newSession = Session.togglePitch pos session

                maybeNote =
                    if not cellIsActive then
                        Just (Session.createPitchNoteAt pos session)
                    else
                        Nothing
            in
            ( { model
                | drawing = newDrawing
                , history = History.push newSession model.history
              }
            , playMaybeNote maybeNote
            )

        ContinueDrawingPitch pos ->
            case Drawing.continuePitch model.drawing of
                Just shouldActivate ->
                    let
                        newSession = Session.setPitch pos shouldActivate session
                        maybeNote = if shouldActivate then Just (Session.createPitchNoteAt pos session) else Nothing
                    in
                    ( { model | history = History.push newSession model.history }
                    , playMaybeNote maybeNote
                    )
                Nothing ->
                    ( model, Cmd.none )

        TimeSync audioContextTime ->
            let
                (newSequencer, stepsToPlay) =
                    Sequencer.tick audioContextTime model.sequencer

                notesToPlay =
                    stepsToPlay
                        |> List.concatMap (\stepIdx -> Session.getNotesForStep stepIdx session)
            in
            ( { model | sequencer = newSequencer, audioContextTime = audioContextTime }
            , Cmd.batch (List.map playNote notesToPlay)
            )

        Play ->
            let
                totalSteps = Session.totalSteps session
                stepDuration = Session.stepDuration session
                newSequencer = Sequencer.play
                    { totalSteps = totalSteps
                    , stepDuration = stepDuration
                    , currentTime = model.audioContextTime
                    }
                    model.sequencer
            in
            ( { model | sequencer = newSequencer }, Cmd.none )

        Undo ->
            ( { model | history = History.undo model.history }, Cmd.none )

        Redo ->
            ( { model | history = History.redo model.history }, Cmd.none )

        ChangeScaleType newScaleType ->
            let
                newSession = Session.setScaleType newScaleType session
            in
            ( { model | history = History.push newSession model.history }, Cmd.none )

        -- ... other messages
```

**Benefits:**
- ✅ Main coordinates, doesn't implement domain logic
- ✅ Clear data flow: Model → Session → Domain modules
- ✅ Each concern isolated in its module
- ✅ Update functions are simple delegations
- ✅ Easy to test: mock Session/Sequencer/Drawing

**Migration notes:**
- Replace Model with smaller Model containing History Session
- Delegate to Session/Sequencer/Drawing instead of implementing
- View code mostly unchanged (ViewModel becomes queries on Session)

---

### 9. Other Modules (Minimal Changes)

**Scales.elm** - No changes needed
- Already well-designed
- No dependencies
- Pure functions
- Good encapsulation

**Timing.elm** - No changes needed
- Already well-designed
- No dependencies
- Pure functions

**Songs.elm** - Minor changes
- Change from returning SongConfig to returning Session
- Use Session.loadSong builder pattern instead

**Utils.elm** - No changes needed
- Generic helpers
- No domain knowledge

---

## MIGRATION IMPACT ANALYSIS

### Adding New Instrument

**Current approach (5 locations):**

1. Add constructor to `DrumKit` type (Instruments.elm:99-104)
2. Update `drumKitConfig` case expression (Instruments.elm:124-153)
3. Update `parseDrumKit` case expression (Instruments.elm:156-172)
4. Update `drumKitLabel` case expression (Instruments.elm:175-188)
5. Update `percLabel` nested case expression (Instruments.elm:196-229)

**New approach (1 location):**

1. Add one entry to `allDrumKitData` list (Instruments.elm):
```elm
, { id = "tabla"
  , label = "Tabla"
  , accentMidi = 65
  , accentWebAudioFont = "_drum_65_0_SBLive_sf2"
  , accentLabel = "Na"
  , bassMidi = 66
  , bassWebAudioFont = "_drum_66_0_SBLive_sf2"
  , bassLabel = "Ge"
  }
```

**Reduction: 5 locations → 1 location (80% reduction)**

---

### Adding New Session Field

**Current approach (11 locations):**

Per Model.elm:122-149 comment:

1. Model type
2. HistoryState type
3. QueryParams type (as Maybe Type)
4. toHistoryState function
5. updateModelFromHistoryState function
6. applyQueryDefaults function
7. applyQueryParams function
8. queryParser function
9. buildQuery function
10. applySong function
11. SongConfig type in Songs.elm (if applicable)

**New approach (3 locations):**

1. Session type (add field)
2. Session module (add getter/setter if needed)
3. Codec.elm encode/decode (if field should be in URL)

**Reduction: 11 locations → 3 locations (73% reduction)**

**Example: Adding "swing" parameter**

Current (11 changes):
```elm
-- 1. Model.elm - Model type
type alias Model =
    { -- ... existing fields ...
    , swing : Float  -- ADD
    }

-- 2. Model.elm - HistoryState type
type alias HistoryState =
    { -- ... existing fields ...
    , swing : Float  -- ADD
    }

-- 3. QuerystringCodec.elm - QueryParams type
type alias QueryParams =
    { -- ... existing fields ...
    , swing : Maybe Float  -- ADD
    }

-- 4. Model.elm - toHistoryState
toHistoryState model =
    { -- ... existing fields ...
    , swing = model.swing  -- ADD
    }

-- 5. Model.elm - updateModelFromHistoryState
updateModelFromHistoryState historyState model =
    { model
        | -- ... existing fields ...
        , swing = historyState.swing  -- ADD
    }

-- 6. QuerystringCodec.elm - applyQueryDefaults
applyQueryDefaults data =
    { data
        | -- ... existing fields ...
        , swing = 0.0  -- ADD
    }

-- 7. QuerystringCodec.elm - load function
load url data =
    { data
        | -- ... existing fields ...
        , swing = Maybe.withDefault data.swing params.swing  -- ADD
    }

-- 8. QuerystringCodec.elm - queryParser
queryParser =
    Pipeline.succeed QueryParams
        |> -- ... existing fields ...
        |> Pipeline.optional (Query.float "swing")  -- ADD

-- 9. QuerystringCodec.elm - buildQuery
buildQuery data =
    UB.absolute []
        [ -- ... existing fields ...
        , UB.float "swing" data.swing  -- ADD
        ]

-- 10. Model.elm - applySong
applySong sc model =
    { model
        | -- ... existing fields ...
        , swing = sc.swing  -- ADD
    }

-- 11. Songs.elm - SongConfig type
type alias SongConfig =
    { -- ... existing fields ...
    , swing : Float  -- ADD
    }
```

New (3 changes):
```elm
-- 1. Session.elm - Session type
type Session
    = Session
        { -- ... existing fields ...
        , swing : Float  -- ADD
        }

-- 2. Session.elm - getter/setter (if needed)
swing : Session -> Float
swing (Session s) = s.swing

setSwing : Float -> Session -> Session
setSwing value (Session s) = Session { s | swing = value }

-- 3. Codec.elm - encode/decode
encode session =
    UB.absolute []
        [ -- ... existing fields ...
        , UB.float "swing" (Session.swing session)  -- ADD in encode
        ]

queryParser =
    -- ... add Query.float "swing" to parser ...  -- ADD in decode
```

---

### Module Dependency Comparison

**Current dependencies:**

```
Grid → Instruments, Scales, Timing (PROBLEM: infrastructure → domain)
Model → Grid, Scales, Instruments, Timing, Songs, QuerystringCodec (PROBLEM: depends on everything)
QuerystringCodec → Grid, Instruments, Scales (PROBLEM: codec knows domain details)
Main → Grid, Model, Scales, Instruments
```

**New dependencies:**

```
Grid → (none - fully generic)
History → (none - fully generic)
Drawing → (none - fully generic)
Sequencer → (none - fully generic)
Session → Grid, Scales, Instruments, Timing (OK: domain → infrastructure + domain)
Codec → Session, Grid, Scales, Instruments (OK: integration → domain)
Main → Session, History, Drawing, Sequencer, Codec (OK: orchestrator → everything)
```

**Improvements:**
- ✅ Grid is now generic (no domain dependencies)
- ✅ 3 new generic modules (History, Drawing, Sequencer) with zero dependencies
- ✅ Session is the only module that coordinates domain concepts
- ✅ Clear layering: Infrastructure → Domain → Integration → Orchestration

---

### Lines of Code Comparison

| Module | Current | New | Change |
|--------|---------|-----|--------|
| Model.elm | 878 | **DELETED** | -878 |
| Session.elm | - | 250 | +250 |
| Sequencer.elm | - | 80 | +80 |
| Drawing.elm | - | 30 | +30 |
| History.elm | - | 40 | +40 |
| Grid.elm | 290 | 50 | -240 |
| Instruments.elm | 301 | 150 | -151 |
| QuerystringCodec.elm | 157 | **DELETED** | -157 |
| Codec.elm | - | 100 | +100 |
| Main.elm | 848 | 600 | -248 |
| **TOTAL** | **2474** | **1300** | **-1174** |

**Net reduction: 47% fewer lines of code**

---

## IMPLEMENTATION PLAN

### Phase 1: Foundation (Independent modules first)

**Estimated time: 1-2 hours**

**Goal:** Create modules with zero dependencies that can be tested in isolation

1. ✅ **Create History module** (~40 lines)
   - Generic undo/redo for any state
   - Write tests
   - Compile check: `elm make src/History.elm --output=NUL`

2. ✅ **Create Drawing module** (~30 lines)
   - State machine for drawing interactions
   - Write tests
   - Compile check: `elm make src/Drawing.elm --output=NUL`

3. ✅ **Refactor Grid module** (~50 lines)
   - Remove Instruments, Scales, Timing dependencies
   - Make fully opaque
   - Update tests
   - Compile check: `elm make src/Grid.elm --output=NUL`

**Milestone:** 3 independent, tested, compiled modules with no dependencies

---

### Phase 2: Domain Modules

**Estimated time: 2-3 hours**

**Goal:** Refactor domain modules to be data-driven and create Session

4. ✅ **Refactor Instruments to data-driven** (~150 lines)
   - Convert TonalInstrument/DrumKit to opaque types
   - Replace case expressions with data lookups
   - Add createPitchNote/createPercNote factories
   - Update tests
   - Compile check: `elm make src/Instruments.elm --output=NUL`

5. ✅ **Create Sequencer module** (~80 lines)
   - Extract playback logic from Model.elm
   - Write tests
   - Compile check: `elm make src/Sequencer.elm --output=NUL`

6. ✅ **Create Session module** (~250 lines)
   - Extract editing state from Model.elm
   - Use Grid, Scales, Instruments, Timing
   - Implement all queries and updates
   - Write tests
   - Compile check: `elm make src/Session.elm --output=NUL`

**Milestone:** All domain modules refactored, Session module created

---

### Phase 3: Integration

**Estimated time: 1-2 hours**

**Goal:** Connect new modules and update orchestration

7. ✅ **Create Codec module** (~100 lines)
   - Simplify QuerystringCodec.elm
   - Work directly with Session
   - Write tests
   - Compile check: `elm make src/Codec.elm --output=NUL`

8. ✅ **Refactor Main.elm** (~600 lines)
   - Update Model type to use History Session
   - Update init to use Codec
   - Update update functions to delegate to Session/Sequencer/Drawing
   - Update view to query Session
   - Compile check: `elm make src/Main.elm --output=NUL`

9. ✅ **Delete old files**
   - Delete Model.elm
   - Delete QuerystringCodec.elm (replaced by Codec.elm)
   - Full compile check: `elm make src/Main.elm --output=NUL`

**Milestone:** Application compiles with new architecture

---

### Phase 4: Cleanup & Verification

**Estimated time: 1 hour**

**Goal:** Ensure everything works and clean up

10. ✅ **Update Songs module** (~200 lines)
    - Change to use Session builder pattern
    - Update all preset songs
    - Test loading songs

11. ✅ **Full application testing**
    - Test all features manually
    - Check undo/redo
    - Check URL serialization
    - Check drawing
    - Check playback
    - Check instrument switching

12. ✅ **Documentation**
    - Update README if needed
    - Add module documentation
    - Mark OverhaulPlan.md as COMPLETED

**Milestone:** Fully working application with new architecture

---

### Estimated Total Time

**4-8 hours of focused work**

- Phase 1: 1-2 hours
- Phase 2: 2-3 hours
- Phase 3: 1-2 hours
- Phase 4: 1 hour

**Note:** Each phase produces working, tested code. We can pause between phases.

---

## OPEN QUESTIONS

### Questions to Resolve Before Implementation

1. **Grid serialization format**
   - Current: Flat comma-separated: "60,0,62,1,64,2" (midi,step,midi,step,...)
   - Keep current format for backward compatibility?
   - **Decision needed:** Keep current format or use JSON?

2. **Session opaqueness**
   - Should Session be fully opaque, or expose fields via getters only?
   - Current design: Fully opaque with getter functions
   - **Decision needed:** Confirm this approach

3. **NoteToPlay type ownership**
   - Currently in Model.elm
   - Proposed: Move to Instruments module (since it creates notes)
   - Alternative: Create separate Audio module?
   - **Decision needed:** Where should NoteToPlay live?

4. **Backwards compatibility for URLs**
   - Should we maintain URL compatibility with current version?
   - If yes, need to keep same query param names
   - **Decision needed:** Confirm we should maintain compatibility

5. **View Model**
   - Current Model.elm has `toVm` that creates ViewModel for view
   - Should this stay in Session, move to Main, or separate View module?
   - **Decision needed:** Where should view model conversion live?

6. **Percussion type extensibility**
   - Currently PercType is custom type: `Accent | Bass`
   - Should we make it data-driven like DrumKit?
   - Probably not needed (only 2 values, unlikely to grow)
   - **Decision needed:** Keep PercType as custom type?

7. **Song loading**
   - Should Songs.elm return Session directly, or return data that Session consumes?
   - Current: Returns SongConfig record
   - Proposed: Return Session? Or keep SongConfig?
   - **Decision needed:** What should Songs module return?

8. **Testing strategy**
   - Write tests during implementation or after?
   - Focus on unit tests per module or integration tests?
   - **Decision needed:** Testing approach

---

### Notes

- This plan prioritizes **encapsulation** and **minimal coupling**
- Each module has **clear ownership** of its concern
- Adding features should touch **1-3 places**, not 11
- Generic modules (Grid, History, Drawing, Sequencer) are **reusable**
- Domain modules (Session, Instruments, Scales) are **data-driven** where possible
- All changes are **incremental and testable** at each phase

---

**STATUS: AWAITING APPROVAL TO PROCEED**

Once open questions are resolved, we can begin Phase 1 implementation.