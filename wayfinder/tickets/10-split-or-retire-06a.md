# Split or retire 06a (wildfire superseded approach)

- **Type:** `wayfinder:grilling`
- **Status:** resolved
- **Assignee:** resolved (work-through session, 2026-08-20)
- **Blocked by:** —
- **Blocks:** —
- **Parent map:** [MAP](../MAP.md)
- **Surfaced by:** 06a vs 06b review during the refactor build-out

## Question

`06a_output_wildfire.R` is the **superseded** wildfire approach (BC Data Catalogue API pull of fire perimeters), while `06b_output_wildfire.R` is the **production** path — it writes the final LAN products (`BC_WILDFIRE_2011_2025.csv`, `BC_CSD_WILDFIRE_2011_2025.csv`) from the geodata team's files + SQL income/GCS. Decide 06a's fate:

1. **Retire** — delete `06a` (or move to `analysis/` for reference, per ADR-0002's out-of-pipeline convention), leaving `06b` as the sole wildfire script. Cleanest if the BCDC-API approach is truly dead.
2. **Split** — action 06a's own header TODO: it contains *two internal approaches* (lines 1–493 BCDC API; 495–716 direct file + SQL), suggesting it be split into `06a` (API) / `06b`-style (direct). Only worth it if both paths still have users.
3. **Consolidate** — extract the DA-intersection transform **duplicated between 06a and 06b** (marked `TODO [DUPLICATE CODE]` in 06b) into `R/transformations.R` (per ADR-0009), then decide (1) or (2) with the shared logic in one place.

Inputs to the decision: does anything downstream still consume 06a outputs? Is anyone still comparing the two approaches? `src/README.md` currently documents both as a pair.

## Resolution

Resolved 2026-08-20 (work-through session). The ticket's premise — 06a as "superseded" — was wrong: **keep both scripts, with distinct roles.**

- **D1 — Keep 06a runnable in `src/` (neither retire nor split).** The BC Data Catalogue updates its fire-perimeter data, and the local geodata file can lag it (it currently does). 06a is the BCDC-API pull and remains the only pipeline path to the catalogue source when it is fresher. Nothing downstream consumes 06a outputs, so it coexists without cost; verified no script/test/app references its `BC_DA_*` outputs.
- **D2 — The duplicated DA-intersection transform lives in 06b alone.** No extraction: 06b is the single production copy, which retires the duplication the natural way; ADR-0009 rules out extracting repeated transforms unless a test demands it. Revisit only if 06b's copy gains a regression test.
- **D3 — The 2026-08-20 06a fixes are committed** (`st_write` overwrite via `delete_dsn`, data-derived year-range output filenames, dictionary label realignment to actual columns) on `wayfinder/refactor-best-practices`; merge to `main` with the rest of the refactor branch, per driver's instruction.

Follow-through recorded: `src/README.md` wildfire rows now describe the two sources' roles (was misleadingly "two parts"); `CONTEXT.md` gains Catalogue-vs-local perimeter terms.
