# Design Principles

## Core Goals
1. **Eliminate long chains across modules** - Sign of bad encapsulation
2. **Modules work on data they encapsulate** - Clear ownership and responsibility
3. **Minimize exposed footprint** - Hide implementation details, expose minimal API
4. **Make impossible states impossible** - Use types to prevent invalid states

## Non-Goals
- Small module size (size doesn't matter)
- Code reusability across projects (not building a library)
- Performance optimization (focus on readability and complexity reduction)
- Fewer update points (nice-to-have, not primary)

## Grouping Principle
**Group fields when:**
- Fields are always operated together
- Other modules consume them as a group
- Grouping is clearly reasonable/stable

**Keep flat when:**
- Fields sometimes operated independently
- Clients want arbitrary access
- Can't predict if grouping will remain stable

## Encapsulation
- Modules define opaque types and provide operations
- Hide implementation details completely
- Type alias vs opaque: opaque enforces discipline, type alias requires client discipline
- Module using type alias for model must still provide API as if opaque

## Abstraction Purpose
- **For**: Decoupling and encapsulation
- **Not for**: Speed optimization
- Precomputed configs are about reducing complexity from coupling, not performance

## Pure Functional Architecture
- All modules are pure (stateless)
- Root module holds all state references
- "Owns data" = module defines opaque type, controls access, hides implementation

## Data-Driven Design
- Replace case expressions with data lookups
- Single source of truth (adding entry vs modifying multiple locations)

## Coordinator Pattern
- When a module references 2+ encapsulated types, it acts as coordinator
- Coordinator is not a god object - it manages cohesive concerns

## ViewModel Pattern
- Facade/config to decouple view from domain modules
- Lives in root module to prevent circular dependencies
- Avoids parameter drilling (10+ params → 1 ViewModel)
- Can contain functions and derived data (not just primitives)

## Type Safety
- Prefer type aliases over raw types: `Set (RowIdx, ColIdx)` not `Set (Int, Int)`
- Clients must use module's exposed functions, never internal details
- Encapsulation applies regardless of type alias vs custom type choice