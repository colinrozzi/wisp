# Changes

This directory tracks concrete, in-progress implementation work.

## Purpose

While `/docs/proposals/` contains high-level vision documents, this directory contains **actionable implementation plans** for changes currently being worked on.

## Structure

Each change document should include:

1. **Header**
   - Status (In Progress, Completed, Paused)
   - Start date
   - Related proposals

2. **Objective** - What are we building?

3. **Motivation** - Why are we building it?

4. **Current State** - What exists today?

5. **Target State** - What will exist when done?

6. **Implementation Plan** - Step-by-step plan with checkboxes
   - Update checkboxes as work progresses
   - Keep this section current!

7. **Examples** - Before/after code samples

8. **Breaking Changes** - What will break?

9. **Open Questions** - Decisions that need to be made

10. **Success Criteria** - How do we know we're done?

## Lifecycle

1. **Created** - New change document, status "In Progress"
2. **Updated** - Checkboxes marked off as work completes
3. **Completed** - All success criteria met, status updated
4. **Archived** - Moved to a subdirectory after completion (optional)

## Active Changes

- [PHASE-0-WASM-INSTRUCTIONS.md](./PHASE-0-WASM-INSTRUCTIONS.md) - Exposing core WASM instructions

## Tips

- Keep documents focused - one change per document
- Update checkboxes in real-time as you work
- Link to related proposals for context
- Include migration guides for breaking changes
- Mark status clearly at the top
