# Modularization scope

- **Type:** `wayfinder:grilling`
- **Status:** resolved
- **Assignee:** resolved (work-through session)
- **Blocked by:** ~~#1 (testability audit)~~ — resolved
- **Blocks:** —
- **Parent map:** [MAP](../MAP.md)

## Question

Decide what duplicated logic to extract into testable functions, informed by #1. Scope strictly to what reduces regression risk or unlocks a test — not a general refactor.

## Resolution

Resolved. See [ADR-0009](../../docs/adr/0009-modularization-scope.md). **Tests + shared infra helpers** — exactly four extractions, into `R/` (sourceable by both scripts and tests):

1. **CHSADA population-weighting algorithm** — from `12_output_CHSA_DA_lookup.R:501–543` (the most complex pure logic in the pipeline, currently an inline pipe chain) → pure function in `R/`, unit-tested (feeds [#3](03-testing-strategy.md) Tier 1).
2. **Relocate the 3 already-pure functions** — `parse_speed()` / `compute_combined_max()` (`14_connectivity.R`) and `remove_geographies()` (`15_…`) → `R/` so `tests/testthat/` can source them cleanly (scripts keep working via `utils.R` or direct source).
3. **`connect_db()`** — deduplicates the `dbConnect` boilerplate repeated across **6 scripts**, and **fixes the inconsistency** where `06b`/`10`/`12` read `config$database$*` but `15` reads `config$data_server$*` — same connection, two config keys (the #9 drift class). Extraction normalizes on one key.
4. **`load_year_config()`** — reads `config_year.yml` and runs `validate_refresh()` (from [#4](04-config-architecture.md)). Avoids duplicating the load+validate pattern across 17 scripts as #4's migration lands.

**Out (per ADR-0004's "no general refactor"):** map-plotting helpers, download-with-fallback, repeated transforms — left inline unless a future need ties them to a test.
