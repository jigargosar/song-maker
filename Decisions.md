# Architecture Decisions

**Goal:** Document the planned architecture - what modules exist, their responsibilities, and design rationale.

## Module Structure

### Grid Module
- **Type**: Opaque `type Grid = Grid (Set (Int, Int))`
- **Purpose**: Infrastructure helper for (Int,Int) storage
- **Decision**: Not generic - specific to (Int,Int) with URL serialization
- **Rationale**: Serialization is grid-specific, no need for false generality

### PitchGrid & PercGrid Modules
- **Decision**: Separate modules instead of single Grid type
- **Rationale**: Different musical concepts with different operations
  - PitchGrid: melody domain, supports transpose
  - PercGrid: rhythm domain, no transpose
  - Sharing (Int,Int) internally is implementation detail
- **PitchGrid encapsulates**: grid + scaleConfig + tonalInstrument
- **PercGrid encapsulates**: grid + drumKit

### Session Module
- **Encapsulates**: pitchGrid + percGrid + timeConfig
- **Rationale**: Groups modules related to history management and serialization and delegates them

### Root Model Structure
- **Location**: Main.elm
- **Fields**:
  - `session : History Session` - Undoable editing state
  - `sequencer : Sequencer` - Playback state machine
  - `drawing : Drawing` - Drawing state machine
  - `audioContextTime : Float` - Ephemeral playback time
  - `url : Url` - Browser URL state
  - `key : Nav.Key` - Navigation key
- **Rationale**: Only Session is history-tracked; sequencer/drawing are state machines; infrastructure (time, url, key) is ephemeral

### Instrument Modules
- **Decision**: Split into TonalInstruments + PercussionInstruments
- **Rationale**: Symmetry with PitchGrid/PercGrid split
- **Implementation**: Data-driven with `allTonalData` and `allDrumKitData` lists
- **Adding instrument**: One entry in data list vs 5 case expression updates
- **PercType**: Expose constructors `PercType(..)` - view needs pattern matching for symbols/positioning

### Config Grouping
- **ScaleConfig**: Grouped (rootNote, scaleType, numPitches, startingOctave)
  - Always operated together by Scales module
- **TimeConfig**: Grouped (bpm, beatsPerMeasure, beatUnit, numMeasures)
  - Always operated together by Timing module
- **tonalInstrument/drumKit**: NOT grouped - operated independently

### ViewModel
- **Location**: Main.elm
- **Rationale**: Prevents circular dependencies, view depends on all domain modules
- **Purpose**: Decouple view from domain, avoid parameter drilling

### History Module
- **Decision**: Generic `History state` wrapper
- **Rationale**: Avoid duplicating Model structure in HistoryState

## Benefits Over Current Implementation

1. Eliminates cross-module chains (Model → Scales + Timing + Grid)
2. Modules operate on data they encapsulate (PitchGrid owns scaleConfig)
3. Adding field to PitchGrid doesn't require updating Model
4. Serialization delegated to child modules (reduces duplication)
5. History wraps only Session (not entire 19-field Model)
6. ScaleConfig/TimeConfig grouped (passed as unit, not 4 separate params)
7. Grid operations don't require external config parameters
8. State machines (Drawing, Sequencer) separated from domain state
9. Type system enforces history tracking (can't modify Session without History)
