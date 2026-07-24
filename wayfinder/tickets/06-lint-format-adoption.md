# lint/format adoption

- **Type:** `wayfinder:grilling`
- **Status:** open
- **Assignee:** unclaimed
- **Blocked by:** —
- **Blocks:** —
- **Parent map:** [MAP](../MAP.md)

## Question

Decide `{lintr}` / `{styler}` adoption:

- **Fix existing violations first** (one dedicated styling/lint pass) or adopt incrementally (enforce only on changed lines)?
- Hard CI **gate** (blocks the PR) or **advisory** (warns)?
- Establish the `.lintr` config and the style baseline.

Note: the codebase is **not** currently lint/style-clean, so this decision determines whether the first CI run is green or red. Resolve via `/grilling` + `/domain-modeling`; record the answer as a resolution comment, then close and add a one-line gist to the map's Decisions so far.
