# Modularization scope: tests + shared infra helpers (four extractions)

Modularization is scoped to **exactly four extractions**, each serving either a test or a deduplication that removes a drift risk — into `R/` where both scripts and `tests/testthat/` can source them:

1. **CHSADA population-weighting algorithm** (`12_output_CHSA_DA_lookup.R:501–543`) — the pipeline's most complex pure logic, currently an inline pipe chain — becomes a pure, unit-tested function.
2. **The three already-pure functions** (`parse_speed`, `compute_combined_max` in `14_connectivity.R`; `remove_geographies` in `15_…`) relocate to `R/` so tests can source them.
3. **`connect_db()`** — the `dbConnect` boilerplate repeats across 6 scripts with an *inconsistent* config key (`config$database$*` in `06b`/`10`/`12` vs `config$data_server$*` in `15`). Same connection, two keys: the exact drift class that produced the `06b` stale-snapshot bug. Extraction normalizes on one key.
4. **`load_year_config()`** — reads `config_year.yml` and runs `validate_refresh()` (ADR-0005), so the load+validate pattern isn't duplicated across 17 scripts as the config migration lands.

**Rationale:** every extraction is justified by either unlocking a unit test (ADR-0008's Tier 1) or eliminating an observed duplication with drift risk. A tests-only scope would leave the `dbConnect` inconsistency and the impending 17× config-load duplication on the table; a broad sweep (map-plotting, download-fallback, repeated transforms) would violate the incremental-not-rearchitecture boundary (ADR-0004) with churn that buys no regression safety.

Detail lives in wayfinder ticket [#7](../../wayfinder/tickets/07-modularization-scope.md).
