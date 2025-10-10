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

### Root Model Structure
- **Location**: Main.elm
- **Fields**:
  - `pitchGrid : PitchGrid` - Melody state
  - `percGrid : PercGrid` - Rhythm state
  - `timeConfig : TimeConfig` - Time structure
  - `undoHistory : List HistoryState` - Past states
  - `redoHistory : List HistoryState` - Future states
  - `sequencer : Sequencer` - Playback state machine
  - `drawing : Drawing` - Drawing state machine
  - `audioContextTime : Float` - Ephemeral playback time
  - `url : Url` - Browser URL state
  - `key : Nav.Key` - Navigation key
- **Rationale**: Domain state flat but grouped (3 fields instead of 12); history tracks HistoryState; state machines and infrastructure separate
- **Serialization**: Model delegates to PitchGrid.serialize, PercGrid.serialize, TimeConfig.serialize

### History State
- **Type**: Record with undoable fields
- **Fields**: pitchGrid, percGrid, timeConfig
- **Rationale**: Mirrors Model's domain fields (3 grouped instead of 12 flat); still duplication but much reduced

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

## Benefits Over Current Implementation

Reordered by impact:

High impact (directly address core goals):
1. Eliminates cross-module chains (Model → Scales + Timing + Grid)
2. Modules operate on data they encapsulate (PitchGrid owns scaleConfig)
3. Operations work on encapsulated data (no external config passing)

Medium impact (reduce complexity/coupling):
4. ScaleConfig/TimeConfig grouped (passed as unit, not 4 separate params)
5. Serialization delegated to child modules (reduces duplication)

Low impact (organizational improvements):
6. Adding field to PitchGrid doesn't require updating Model
7. State machines (Drawing, Sequencer) separated from domain state
8. HistoryState has 3 grouped fields instead of 12 flat fields
