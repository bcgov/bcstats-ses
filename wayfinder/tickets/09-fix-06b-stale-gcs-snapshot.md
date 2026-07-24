# Fix 06b stale GCS snapshot (correctness bug)

- **Type:** `wayfinder:task`
- **Status:** resolved
- **Assignee:** resolved (work-through session)
- **Blocked by:** —
- **Blocks:** —
- **Parent map:** [MAP](../MAP.md)
- **Surfaced by:** [#2 Config audit](02-config-audit.md)

## Question (task)

`06b_output_wildfire.R:130` hardcoded the GCS snapshot `FCT_GCS_202509`, bypassing `config_year.yml` (`FCT_GCS_202606`). Confirm intent, then fix.

## Resolution

**Confirmed bug** (leftover, not intentional — no comment, the only hardcoded `FCT_GCS_*` ref; `03`/`04` already use config). Fix applied to `src/06b_output_wildfire.R`:

- Added `year_config <- config::get(file = "config_year.yml")` (L67).
- Changed the income query's GCS table from `[Prod].[FCT_GCS_202509]` to config-driven: `sprintf("...FROM [Population_Labour_Social].[%s].[%s]...", year_config$gcs$schema, year_config$gcs$table)` — same pattern as `03`/`04`.
- R parse: **clean (37 expressions)**. Used `sprintf` rather than `glue("{…}")` to avoid a linter false-positive from braces inside the multi-line SQL string.

⚠️ **Data impact:** `06b` now resolves wildfire geography against `FCT_GCS_202606` (was `202509`) — **wildfire outputs will change**. Action required: re-run `06b` in the secure environment and verify/regenerate downstream wildfire outputs.

⚠️ **Deployment note:** the fix is committed on the `wayfinder/refactor-best-practices` branch. To deploy, move it to an appropriate branch (bugfix off `main` or onto `sei_2024_release`), re-run `06b`, and confirm outputs.

The linter's "symbol not in scope" warnings (dplyr/sf/DBI functions) are pre-existing and file-wide — a `pacman::p_load()` limitation, not from this fix.
