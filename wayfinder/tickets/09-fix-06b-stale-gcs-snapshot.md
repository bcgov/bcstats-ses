# Fix 06b stale GCS snapshot (correctness bug)

- **Type:** `wayfinder:task`
- **Status:** open
- **Assignee:** unclaimed
- **Blocked by:** — (`config_year.yml` already holds the correct value)
- **Blocks:** —
- **Parent map:** [MAP](../MAP.md)
- **Surfaced by:** [#2 Config audit](02-config-audit.md)

## Question (task)

`06b_output_wildfire.R:130` hardcodes the GCS snapshot table `FCT_GCS_202509`, bypassing `config_year.yml` (which specifies `FCT_GCS_202606`). Wildfire data is therefore resolved against an **older geography snapshot than the rest of the pipeline** — a latent correctness inconsistency.

**Fix:** make `06b` read the GCS snapshot table from `config_year.yml` the same way `03`/`04` do (`config::get("gcs")$table`), so it stays in sync across refreshes.

**Before changing, verify** whether `FCT_GCS_202509` was intentional (wildfire-specific reason) or a leftover bug — record that finding in the resolution. This overlaps with #4's systematic migration, but it's a targeted correctness fix worth doing promptly. Record the outcome (fixed / intentional-kept) and any data impact.
