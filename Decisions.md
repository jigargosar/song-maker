# Architecture Decisions

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
- **PitchGrid owns**: grid + scaleConfig + tonalInstrument
- **PercGrid owns**: grid + drumKit

### Session Module
- **Type**: Opaque coordinator
- **Owns**: pitchGrid + percGrid + timeConfig
- **Role**: Coordinates cohesive modules, not god object
- **Serialization**: Session owns its serialization (no separate Codec module)
- **Rationale**: Session already has all dependencies, avoids duplication

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
