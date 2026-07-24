# Testability audit

- **Type:** `wayfinder:research`
- **Status:** resolved (by /research subagent)
- **Assignee:** resolved
- **Blocked by:** —
- **Blocks:** #3 (testing strategy), #5 (CI design), #7 (modularization scope) — now unblocked
- **Parent map:** [MAP](../MAP.md)

## Question

Classify each script in `src/` (`01`–`18`) by how much of its logic is **pure/testable** vs. **coupled to the secure LAN/SQL Server**, and estimate the CI-testable fraction. Note the existing `test/test_crime_rate_regression.R`.

## Resolution

Resolved by /research subagent; full report at [`../research/01-testability-audit.md`](../research/01-testability-audit.md).

- **~10% of pipeline logic is CI-testable** (~800 of ~8,100 lines across 18 scripts + `utils.R`). The pipeline is overwhelmingly I/O-bound: `DBI`/SQL Server queries, `bcdata`/`cansim`/`cancensus` API calls, and `safepaths` LAN reads dominate every script.
- The existing `test/test_crime_rate_regression.R` is a **custom snapshot test (not testthat)** that requires live LAN/DB — it sources script `03` and compares output CSVs; documented local-only, cannot run in CI.
- **Top unit-test candidates** (already pure functions, no refactor needed): `parse_speed()` and `compute_combined_max()` in `14_connectivity.R`; `remove_geographies()` in `15_remove_geo_suppression_ids.R`.
- **Best extraction target:** the CHSADA population-weighting algorithm in `12_output_CHSA_DA_lookup.R` (L501–543) — the most complex pure logic, currently an inline pipe chain.
- `utils.R` has **zero** CI-testable functions (every function does file I/O, DB queries, or `ggsave`).

**Unblocks:** #3 (testing strategy), #5 (CI design), #7 (modularization scope).
