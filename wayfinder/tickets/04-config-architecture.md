# Config architecture & annual-refresh contract

- **Type:** `wayfinder:grilling`
- **Status:** open
- **Assignee:** unclaimed
- **Blocked by:** #2 (config audit)
- **Blocks:** —
- **Parent map:** [MAP](../MAP.md)

## Question

Decide the target `{config}` architecture:

- The split between `config_year.yml` (tracked: year-sensitive, non-secret refresh values) and `config.yml` (gitignored: secrets/credentials) — and exactly what migrates where, per the config audit (#2) inventory.
- The **annual-refresh contract**: the precise, enumerated set of values to bump in `config_year.yml` each year so a refresh is literally "edit config + re-run" — no script editing.

Resolve via `/grilling` + `/domain-modeling`; record the answer as a resolution comment, then close and add a one-line gist to the map's Decisions so far.
