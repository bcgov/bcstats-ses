# CI design

- **Type:** `wayfinder:grilling`
- **Status:** resolved
- **Assignee:** resolved (work-through session)
- **Blocked by:** ~~#1 (testability audit)~~ — resolved
- **Blocks:** —
- **Parent map:** [MAP](../MAP.md)

## Question

Design the GitHub Actions CI that runs on every PR: what runs, the R version matrix, `renv::restore()` caching, and how DB/LAN-dependent tests are gated out.

## Resolution

Both decisions resolved. See [ADR-0007](../../docs/adr/0007-ci-design.md).

**D1 — R version matrix: 4.5.2 + 4.6.1.** 4.5.2 matches `renv.lock` exactly (deterministic restore); 4.6.1 is the developers' local version — the pair catches skew between the pinned lock and newer dev environments.

**D2 — `skip_if_no_db()` helper.** A helper in `tests/testthat/helper.R` (e.g. checks for a DB connection or env var) that each DB/LAN-coupled test calls. CI has no DB/LAN, so those tests self-skip; pure-logic tests run normally. New DB tests inherit the skip for free — knowledge lives in the test, not the workflow. The existing `test/test_crime_rate_regression.R` (explicitly "not CI", needs live DB) moves under this convention.

**CI shape (what runs on every PR):**
1. **Checkout + `r-lib/actions/setup-r@v2`** with the 4.5.2/4.6.1 matrix.
2. **`renv::restore()`** via `r-lib/actions/setup-renv@v2` (with cache).
3. **Lint/format job** (from [#6](06-lint-format-adoption.md)): on **changed `*.R`/`*.r` files only** — `styler` gate (`dry = "fail"`, blocking) + `lintr` advisory (reports, non-blocking).
4. **Test job:** `testthat::test_local()` — pure-logic tests run; DB/LAN tests self-skip via `skip_if_no_db()`.
5. **Explicitly out:** the full pipeline does **not** run in CI (no LAN/SQL Server).

**Prereq / build work:** create `.github/workflows/ci.yml`; add `lintr`+`styler` to `renv.lock` (per #6); scaffold `tests/testthat/` with `helper.R` containing `skip_if_no_db()`; migrate the crime-rate regression test into the testhat structure under the skip convention.
