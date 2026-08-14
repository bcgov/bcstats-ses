# Testing strategy: two tiers — unit (pure, CI) + snapshot (DB-coupled, local)

A single `tests/testthat/` tree with **two tiers**:

1. **Unit tier (runs in CI):** real unit tests for the ~10% pure logic — `parse_speed()`, `compute_combined_max()` (`14_connectivity.R`), `remove_geographies()` (`15_…`), the CHSADA population-weighting algorithm once extracted.
2. **Snapshot tier (runs locally on the LAN, self-skips in CI via `skip_if_no_db()` from ADR-0007):** golden-CSV regression tests for DB-coupled pipelines, generalizing the existing `test/test_crime_rate_regression.R` pattern — run the pipeline, compare outputs to committed golden snapshots.

**Rationale:** the audit (wayfinder #1) found ~90% of the pipeline is I/O-bound (DB/SQL, APIs, LAN reads). Mocking that layer was rejected: high upfront effort, brittle mocks, and — critically — mocks don't catch *real data-shape changes* upstream, which is the actual regression risk in a data-refresh pipeline. The snapshot tier catches exactly that: if a refresh or code change alters an output, the golden comparison fails loudly. Accepting the 90% as untested was rejected for the same reason — the snapshot tier is the safety net the driver (regression safety) demands.

**Consequences:** the crime-rate regression test migrates into `tests/testthat/` under the skip convention; the ad-hoc `test/` dir is cleaned up (`test_id_recipe.r` → `analysis/` or delete); "tested" = every pure function has CI unit tests + every pipeline stage whose output matters has a golden snapshot checked at each annual refresh. Detail lives in wayfinder ticket [#3](../../wayfinder/tickets/03-testing-strategy.md).
