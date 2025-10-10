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

### PercussionInstruments Module
- **Type**: Opaque `type DrumKit`
- **Structure**: DrumConfig (domain), DrumKitVM (view labels/icon shape)
- **Implementation**: Data-driven with internal `allDrumKitData` list
- **API**: Query functions `getDrumKitConfig`, `getDrumKitVM`
- **Icon**: DrumKitVM may contain `type IconShape = Circle | Rect | Triangle` for view case expression. instead of inlining svg 
- **PercType**: Expose constructors `PercType(..)` for view pattern matching
- **Rationale**: Single modification point, opaque type prevents invalid IDs

### TonalInstruments Module
- **Decision**: TBD (likely following same structure as PercussionInstruments)

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

## Implementation Strategy

Start with highest value, least disruption:
1. **PercussionInstruments module** - Self-contained, minimal cross-module impact
2. **TonalInstruments module** - Similar structure to PercussionInstruments
3. **Config grouping** (ScaleConfig/TimeConfig) - Requires Model/HistoryState updates
4. **PitchGrid/PercGrid modules** - Major refactoring, touches serialization and history

## Benefits Over Current Implementation

**High impact (eliminate cross-module chains):**
1. Grid operations no longer require external config parameters (Model → Grid → Scales eliminated)
2. Modules encapsulate their dependencies (PitchGrid owns scaleConfig, operations self-contained)

**Medium impact (reduce duplication/coupling):**
3. HistoryState reduces from 12 flat fields to 3 grouped fields
4. Module serialization eliminates QuerystringCodec duplication
5. Adding grid-related field touches one module instead of Model + History + serialization

**Low impact (organizational improvements):**
6. Opaque instrument types prevent invalid ID construction
7. Domain config separated from view presentation (DrumConfig vs DrumKitVM)
