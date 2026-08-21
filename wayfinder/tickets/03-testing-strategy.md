# Testing strategy

- **Type:** `wayfinder:grilling`
- **Status:** resolved
- **Assignee:** resolved (work-through session)
- **Blocked by:** ~~#1 (testability audit)~~ — resolved
- **Blocks:** —
- **Parent map:** [MAP](../MAP.md)

## Question

Given the testability audit (#1), decide the testing approach: `{testthat}` structure, unit vs. snapshot/regression split, and how to treat DB/LAN-coupled code.

## Resolution

Both decisions resolved. See [ADR-0008](../../docs/adr/0008-testing-strategy.md).

**D1 — Single `tests/testthat/` tree:**
- The existing `test/test_crime_rate_regression.R` migrates into `tests/testthat/` (e.g. `test-regression-crime-rate.R`) as a first-class testhat test, gating itself with `skip_if_no_db()` (per [#5](05-ci-design.md)) — runs locally on the LAN, self-skips in CI.
- Unit tests are added per pure function (`test-parse_speed.R`, `test-compute_combined_max.R`, `test-remove_geographies.R`, …) — these run in CI.
- The ad-hoc `test/` directory is cleaned up: `test_id_recipe.r` is a dev scratch script → move to `analysis/` or delete.

**D2 — Two-tier testing: unit (pure) + snapshot (DB-coupled, local):**
- **Tier 1 — Unit (CI):** real unit tests for the ~10% pure logic — `parse_speed()` / `compute_combined_max()` (`14_connectivity.R`), `remove_geographies()` (`15_…`), and the CHSADA population-weighting algo once extracted ([#7](07-modularization-scope.md)).
- **Tier 2 — Snapshot/regression (local, LAN):** for DB-coupled pipelines — the existing crime-rate golden-CSV pattern generalizes: run the pipeline locally, compare outputs against committed golden snapshots (`test/snapshots/` convention already in place). Gated by `skip_if_no_db()` so CI never touches them. **Not** mock-the-DB: mocks are high-effort, brittle, and don't catch real data-shape changes — the snapshot tier gives the true regression *safety* signal.

**"Tested" means, for this pipeline:** every pure function has unit tests (CI-enforced); every pipeline stage whose output matters gets a golden-snapshot regression test (run at least once per annual refresh, on the LAN).
