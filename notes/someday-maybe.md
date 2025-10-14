# Someday/Maybe

- Revisit why parse functions (parseScaleType, parseRootNote) return defaults instead of Maybe
- Audit and fix incorrect naming: pitchIdx should be noteIdx throughout codebase, check scaleConfig vs scaleRange naming inconsistencies
- Consider if bulk functions (noteNamesInRange) should have optimized implementations vs using single-value functions with map
- Precompute entire pitch grid data (noteIdx, stepIdx, isActive) in ViewModel to eliminate repeated noteIdx→MIDI conversions and TonalGrid lookups
- Audit other VM query patterns (perc rows, step labels, cell-level data) for similar data precomputation opportunities