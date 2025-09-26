# Song Maker V2 - Development Plans & Strategies

 ## Generated Config-Driven Strategy for Simplification of Instrument Loading

### Overview
Replace hardcoded instrument definitions with a dynamic config-driven system that maintains clean APIs while enabling runtime flexibility.

### Architecture Strategy

**Core Approach:**
- **JavaScript Config**: Single source of truth for all instrument definitions
- **Generated Elm Config**: Auto-generated module from JavaScript config
- **Dynamic Lookups**: Instruments.elm queries generated config instead of hardcoded enums

### Implementation Plan

#### 1. Create JavaScript Config
```javascript
// instruments-config.js
export const INSTRUMENTS_CONFIG = {
  tonal: {
    piano: { label: "Piano", webAudioFont: "_tone_0000_SBLive_sf2", url: "https://..." },
    marimba: { label: "Marimba", webAudioFont: "_tone_0120_SBLive_sf2", url: "https://..." },
    strings: { label: "Strings", webAudioFont: "_tone_0400_SBLive_sf2", url: "https://..." }
  },
  drums: {
    electronic: {
      label: "Electronic",
      kick: { webAudioFont: "_drum_36_0_SBLive_sf2", midi: 36, url: "https://..." },
      snare: { webAudioFont: "_drum_38_0_SBLive_sf2", midi: 38, url: "https://..." }
    },
    rock: { /* ... */ }
  }
};
```

#### 2. Generate Elm Config Module
```javascript
// scripts/generate-instrument-config.js
// Generates InstrumentConfig.elm from instruments-config.js
```

```elm
-- Generated: InstrumentConfig.elm (data only)
module InstrumentConfig exposing (..)

tonalConfig : List { key : String, label : String, webAudioFont : String, url : String }
tonalConfig =
    [ { key = "piano", label = "Piano", webAudioFont = "_tone_0000_SBLive_sf2", url = "..." }
    , { key = "marimba", label = "Marimba", webAudioFont = "_tone_0120_SBLive_sf2", url = "..." }
    , { key = "strings", label = "Strings", webAudioFont = "_tone_0400_SBLive_sf2", url = "..." }
    ]

drumConfig : List { key : String, label : String, kick : {...}, snare : {...} }
drumConfig = [...]
```

#### 3. Update Instruments.elm (Dynamic Lookups)
```elm
-- Instruments.elm transitions from static enum-based to dynamic config-based
import InstrumentConfig

-- Types become string wrappers
type TonalInstrument = TonalKey String
type DrumKit = DrumKey String
type PercType = PercKey String

-- Functions query generated config
tonalWebAudioFont : TonalInstrument -> String
tonalWebAudioFont (TonalKey key) =
    InstrumentConfig.tonalConfig
        |> List.filter (.key >> (==) key)
        |> List.head
        |> Maybe.map .webAudioFont
        |> Maybe.withDefault "_tone_0000_SBLive_sf2"

-- Constructor helpers
defaultTonalInstrument : TonalInstrument
defaultTonalInstrument = TonalKey "piano"

percKick : PercType
percKick = PercKey "kick"
```

#### 4. WebAudioFont Loading
```html
<!-- index2.html -->
<script type="module">
  import { INSTRUMENTS_CONFIG } from './instruments-config.js';

  // Use WAF loader for all instruments
  Object.values(INSTRUMENTS_CONFIG.tonal).forEach(instrument => {
    player.loader.startLoad(audioContext, instrument.url, instrument.webAudioFont);
  });

  Object.values(INSTRUMENTS_CONFIG.drums).forEach(kit => {
    player.loader.startLoad(audioContext, kit.kick.url, kit.kick.webAudioFont);
    player.loader.startLoad(audioContext, kit.snare.url, kit.snare.webAudioFont);
  });

  // Wait for loading completion, then init Elm
  player.loader.waitLoad(() => {
    const app = Elm.Main2.init({ node: document.getElementById("root") });
    // ... existing port setup
  });
</script>
```

### Benefits
- ✅ **Single Source of Truth** - All instruments defined in JavaScript config
- ✅ **Clean API Preserved** - External interface unchanged (`tonalWebAudioFont model.currentTonalInstrument`)
- ✅ **Easy Extensions** - Add instrument to config, everything else auto-updates
- ✅ **Proper WAF Usage** - Uses `startLoad`/`waitLoad` pattern as intended
- ✅ **No Circular Dependencies** - Clean module boundaries maintained

### Implementation Steps
1. Create backup: `cp Instruments.elm InstrumentsBackup.elm`
2. Create `instruments-config.js` with current instruments
3. Create generation script for `InstrumentConfig.elm`
4. Update `Instruments.elm` to use dynamic lookups
5. Update `index2.html` to use WAF loader with config
6. Test and refine

---

## 📋 Other Development Plans

### REFACTORING PLAN: Extract MusicTheory Module

#### Goal
Extract pure music theory code into a separate MusicTheory.elm module while maintaining clean boundaries and dependencies.

#### Module Design

**MusicTheory.elm (Pure Functions - No Dependencies)**
```elm
module MusicTheory exposing
    ( RootNote(..)
    , ScaleType(..)
    , allRootNotes
    , allScaleTypes
    , chromaticNoteNames
    , getRootNoteOffset
    , getScalePattern
    , notesPerOctave
    , parseRootNote
    , parseScaleType
    , rootNoteLabel
    , scaleLabel
    )
```

**Types to Extract:**
- `ScaleType` (Major | Pentatonic | Chromatic)
- `RootNote` (C | CSharp | D | DSharp | E | F | FSharp | G | GSharp | A | ASharp | B)

**Functions to Extract:**
- `getScalePattern : ScaleType -> List Int`
- `getRootNoteOffset : RootNote -> Int`
- `chromaticNoteNames : List String`
- `notesPerOctave : ScaleType -> Int`

**New Functions to Add:**
- `scaleLabel : ScaleType -> String` (for UI)
- `rootNoteLabel : RootNote -> String` (for UI)
- `parseScaleType : String -> ScaleType` (with default)
- `parseRootNote : String -> RootNote` (with default)
- `allScaleTypes : List ScaleType`
- `allRootNotes : List RootNote`

#### Main2.elm Updates

**Import:**
```elm
import MusicTheory exposing (ScaleType, RootNote)
```

**Functions to Keep (Model-dependent):**
- `getTotalPitches : Model -> Int` - needs Model.scaleType and Model.octaveRange.count
- `pitchIdxToMidi : Int -> Model -> Int` - needs Model context for calculations
- `pitchIdxToNoteName : Int -> Model -> String` - needs Model context for calculations
- All view functions (UI-specific)
- All parsing functions in view selectors

**Update Function Calls:**
Replace direct calls with module-prefixed versions:
- `getScalePattern model.scaleType` → `MusicTheory.getScalePattern model.scaleType`
- `getRootNoteOffset model.rootNote` → `MusicTheory.getRootNoteOffset model.rootNote`
- `notesPerOctave model.scaleType` → `MusicTheory.notesPerOctave model.scaleType`
- Update view functions to use `MusicTheory.scaleLabel`, `MusicTheory.rootNoteLabel`
- Update selectors to use `MusicTheory.allScaleTypes`, `MusicTheory.allRootNotes`

**Remove from Main2.elm:**
- `ScaleType`, `RootNote` type definitions
- `getScalePattern`, `getRootNoteOffset`, `chromaticNoteNames`, `notesPerOctave` functions
- Hardcoded scale/root note lists and parsing functions
- Move `parseScaleType`, `parseRootNote` to MusicTheory.elm

#### Implementation Steps

1. **Create MusicTheory.elm**:
   - Extract pure types and functions
   - Add missing label and parsing functions
   - Add allScaleTypes, allRootNotes lists
   - Test compilation

2. **Update Main2.elm imports**:
   - Add MusicTheory import with explicit types
   - Update all function calls to use MusicTheory prefix
   - Remove extracted type definitions and functions

3. **Update view functions**:
   - Replace hardcoded lists with MusicTheory.allScaleTypes/allRootNotes
   - Replace hardcoded case expressions with MusicTheory.scaleLabel/rootNoteLabel
   - Update parsing calls to use MusicTheory functions

4. **Test and refine**:
   - Ensure compilation succeeds
   - Verify UI still works correctly
   - Check that no music theory logic remains in Main2.elm

#### Benefits
- **Clean separation**: Music theory vs app-specific logic
- **Reusability**: MusicTheory.elm can be used by other music applications
- **Maintainability**: Easier to extend with new scales, modes, chord progressions
- **Testability**: Pure functions are easy to unit test
- **Consistency**: Follows same pattern as Instruments.elm module

#### Module Boundaries
- **MusicTheory.elm**: Pure music theory concepts, no Model dependencies
- **Main2.elm**: App-specific pitch calculations that require Model context
- **Instruments.elm**: Sound source definitions and instrument-related functions

---

### TODO: Simplify SequenceConfig to Single Steps Input

#### Current Problem
The current SequenceConfig with bars/beatsPerBar/subdivisions is overcomplicated:
- Users need to understand musical theory concepts
- Three inputs (bars, beats, subdivisions) for what should be simple
- Mental overhead of calculating total steps from these values

#### Proposed Simplification
Replace the current SequenceConfig UI with a single "Steps" input:

```elm
-- Keep internal model the same for future extensibility
type alias SequenceConfig =
    { bars : Int, beatsPerBar : Int, subdivisions : Int }

-- But show user just one input
viewStepsInput : Int -> Html Msg  -- Shows getTotalSteps model

-- Handle conversion internally
ChangeSteps newSteps ->
    let
        newConfig = { bars = 1, beatsPerBar = 1, subdivisions = newSteps }
    in
    { model | sequenceConfig = newConfig }
```

#### Benefits
- **Much simpler UI**: One input instead of three
- **User-friendly**: Just "how many steps do you want?"
- **No constraints**: Any step count (7, 13, 37, 128, etc.)
- **No musical theory required**: Users don't need to understand bars/beats
- **Flexible**: 16, 32, 48, 64, 128 steps - whatever they need
- **Future-proof**: Internal structure preserved for advanced features later

#### Implementation
1. Replace viewSequenceControls with single viewStepsInput
2. Add ChangeSteps message handler
3. Remove ChangeBars, ChangeBeatsPerBar, ChangeSubdivisions
4. Keep SequenceConfig type for internal consistency
5. Test with various step counts

This gives maximum user flexibility with minimum UI complexity.

---

### TODO: Improve Octave Controls - Musical Operations vs Workspace Management

#### Current Problems
1. **Poor UX**: Start/Count inputs require mental math (start=3, count=3 = octaves 3,4,5)
2. **Destructive behavior**: Changing Start/Count loses notes outside new range
3. **Missing transpose**: No way to move all notes up/down by octaves
4. **Confusing purpose**: Start/Count conflates workspace management with musical operations

#### Proposed Solution: Separate Musical Operations from Workspace Management

**Musical Operations (Move Notes)**
```elm
-- Transpose all notes by octaves with auto-range-expansion
Transpose: [ ⬆️ Up Octave ] [ ⬇️ Down Octave ]
Key: [C] [C#] [D] ... -- (existing root note selector - already works correctly)

-- Example: Transpose Up
-- Notes: C4,E4,G4 → C5,E5,G5
-- Range: 3,4,5 → auto-expands to 3,4,5,6 if needed
-- Purpose: Move music to different pitch level
```

**Workspace Management (Adjust Available Range)**
```elm
-- Manually control composition workspace
Range: [ ⬆️ Expand Up ] [ ⬇️ Expand Down ] [ ⬆️ Shrink Up ] [ ⬇️ Shrink Down ]
Display: "Octaves 3-5" -- Clear current range indicator

-- Example: Expand Down
-- Range: 3,4,5 → 2,3,4,5
-- Notes: Stay exactly where they are
-- Purpose: Add workspace below existing music for bass/harmony
```

#### Implementation Requirements

**1. Transpose Functions**
```elm
transposeOctaveUp : Model -> (Model, Cmd Msg)
transposeOctaveDown : Model -> (Model, Cmd Msg)

-- Auto-expand range to fit transposed notes
autoExpandRange : PitchGrid -> OctaveRange -> OctaveRange
```

**2. Range Management Functions**
```elm
expandRangeUp : Model -> Model      -- Add octave above
expandRangeDown : Model -> Model    -- Add octave below
shrinkRangeUp : Model -> Model      -- Remove top octave (if empty)
shrinkRangeDown : Model -> Model    -- Remove bottom octave (if empty)
```

**3. Destructive Operation Warnings**
```elm
type OperationWarning
    = WillLoseNotes Int  -- "This will remove N notes"
    | SafeOperation

checkRangeShrink : OctaveRange -> PitchGrid -> OperationWarning

-- UI shows warning dialog before destructive operations:
-- "Shrinking will remove 3 notes. Continue? [Shrink Anyway] [Cancel]"
```

**4. Updated Message Types**
```elm
type Msg
    = -- ... existing messages
    | TransposeOctaveUp
    | TransposeOctaveDown
    | ExpandRangeUp
    | ExpandRangeDown
    | ShrinkRangeUp
    | ShrinkRangeDown
    | ConfirmDestructiveOperation (Model -> Model) -- For warning dialogs
```

**5. Better UI Components**
```elm
viewTransposeControls : Html Msg
viewRangeControls : Model -> Html Msg  -- Show warnings if needed
viewOctaveRangeDisplay : OctaveRange -> Html Msg  -- "Octaves 2-6"
```

#### User Experience Improvements

**Clear Separation of Concerns**
- **"I want to move my music"** → Use Transpose controls
- **"I need more room to compose"** → Use Range controls
- **"I want a different key"** → Use Root note selector

**Non-Destructive by Default**
- Transpose: Auto-expands range to prevent note loss
- Expand: Never loses notes (always safe)
- Shrink: Only allowed if no notes in removed octave, or with user confirmation

**Progressive Disclosure**
- Simple operations (transpose, expand) work immediately
- Destructive operations show warnings with alternatives
- Power users can override warnings if needed

#### Benefits
- **Intuitive**: Operations match how musicians think
- **Safe**: Destructive actions require confirmation
- **Flexible**: Separates musical intent from workspace management
- **Professional**: Matches behavior of modern DAWs and music software
- **Discoverable**: Clear button labels show what each operation does

#### Implementation Steps
1. Transpose controls (most commonly needed)
2. Range expand controls (composition workflow)
3. Destructive operation warnings (safety)
4. Range shrink controls (nice-to-have cleanup)

---

### ✅ COMPLETED: Undo/Redo System Implementation

#### Overview
Add undo/redo functionality to preserve user work and enable experimentation.
Simple history stack approach storing snapshots of undoable state.

#### Core Design

**History State Structure**
```elm
type alias HistoryState =
    { pitchGrid : PitchGrid
    , percGrid : PercGrid
    , scaleType : ScaleType
    , rootNote : RootNote
    , octaveRange : { start : Int, count : Int }
    , sequenceConfig : SequenceConfig
    }
```

**Model Changes**
```elm
type alias Model =
    { -- existing fields...
    , undoStack : List HistoryState
    , redoStack : List HistoryState
    }
```

**Message Types**
```elm
type Msg
    = -- existing messages...
    | Undo
    | Redo
```

#### State Classification

**Undoable Operations (Musical State)**
- Grid modifications (drawing notes, clearing)
- Scale/key changes (ChangeScaleType, ChangeRootNote)
- Range changes (ChangeOctaveStart, ChangeOctaveCount, transpose operations)
- Sequence structure (ChangeBars, ChangeBeatsPerBar, ChangeSubdivisions)

**Non-Undoable Operations (UI/Temporary State)**
- Drawing state (StartDrawing, ContinueDrawing, StopDrawing)
- Playback state (Play, Stop, TimeSync)
- Audio context time updates
- Instrument/kit selection (instrument changes don't affect composition)
- BPM changes (performance setting, not composition structure)

#### Implementation Status
- ✅ **History State Structure** - HistoryState type defined
- ✅ **Model Changes** - undoStack and redoStack added to Model
- ✅ **History Management Functions** - pushToHistory implemented
- ✅ **Update Logic Integration** - All undoable operations capture history
- ✅ **Undo/Redo Handlers** - Message handlers implemented
- ✅ **UI Integration** - Undo/Redo buttons in footer with proper disabled states

#### Memory Considerations
- Limit history size (e.g., 50 operations) to prevent memory issues
- Clear redo stack on new operations (standard behavior)

---

### TODO: Root Note Change Preservation
Currently when changing root note the resize grid function chops off notes.
Ideally it should preserve notes, since changing root note doesn't change the number of pitches.

---

## 📝 Development Notes

### Architecture Principles
- **Clean APIs**: External interfaces should remain simple and intuitive
- **Single Source of Truth**: Config data should live in one place
- **Type Safety**: Elm's type system should catch configuration errors
- **Progressive Enhancement**: New features shouldn't break existing functionality
- **Module Boundaries**: Clear separation of concerns between modules

### Current Status
- ✅ Undo/Redo system fully implemented and working
- ✅ Constructor exposure eliminated - clean module boundaries established
- ✅ Percussion naming improved (`percKick`, `percSnare`)
- 🎯 **Next Priority**: Config-driven instrument system implementation