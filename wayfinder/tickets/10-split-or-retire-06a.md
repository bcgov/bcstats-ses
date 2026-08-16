# Split or retire 06a (wildfire superseded approach)

- **Type:** `wayfinder:grilling`
- **Status:** open
- **Assignee:** unclaimed
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
