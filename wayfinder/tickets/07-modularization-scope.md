# Modularization scope

- **Type:** `wayfinder:grilling`
- **Status:** open
- **Assignee:** unclaimed
- **Blocked by:** #1 (testability audit)
- **Blocks:** —
- **Parent map:** [MAP](../MAP.md)

## Question

Decide what duplicated logic to extract into testable functions (`utils.R` or per-stage helpers), informed by the testability audit (#1).

Scope strictly to what **reduces regression risk** or **unlocks a test** — not a general refactor. Identify the top extraction candidates (likely: repeated DB-connect patterns, the map-plotting helpers in `utils.R`, repeated transforms across `01`–`14`). Resolve via `/grilling` + `/domain-modeling`; record the answer as a resolution comment, then close and add a one-line gist to the map's Decisions so far.
