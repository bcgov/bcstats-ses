# CI design: R matrix 4.5.2+4.6.1, skip_if_no_db() gating, changed-files lint gate

GitHub Actions CI runs on every PR. R matrix is **4.5.2** (matches `renv.lock` exactly — deterministic `renv::restore()`) **+ 4.6.1** (developers' local version — catches lock/dev skew). DB/LAN-coupled tests self-skip via a **`skip_if_no_db()` helper** in `tests/testthat/helper.R`; CI has no DB/LAN, so those tests skip automatically while pure-logic tests run.

**The PR workflow runs:**
1. Checkout + `setup-r` (4.5.2/4.6.1 matrix) + `setup-renv` (cached restore).
2. **Lint/format** (per ADR-0006): on **changed files only** — `styler` gate (blocking, `dry="fail"`) + `lintr` advisory (non-blocking).
3. **Tests:** `testthat::test_local()` — pure tests run, DB/LAN tests self-skip.

**Rationale:** the pipeline depends on a secure LAN/SQL Server and cannot run end-to-end in CI, so CI guards *code quality + testable logic only* — never the full pipeline. `skip_if_no_db()` puts the gating knowledge in the *test* (which knows it needs a DB), not the workflow (which would have to maintain a path allowlist or skip-list that drifts as files are added). The dual R version catches the real-world skew between the pinned lock and devs' newer machines without the overhead of a full release/oldrel/devel matrix.

**Considered:**
- Single version (4.5.2 only) — simpler, but silent on lock/dev skew.
- Full release/oldrel/devel matrix — CRAN-package overhead; little regression-safety payoff for an internal pipeline.
- Subdir allowlist or workflow skip-list for DB tests — rejected: gating knowledge lives outside the test, drifts as files are added/renamed.

**Consequences:** create `.github/workflows/ci.yml`; add `lintr`+`styler` to `renv.lock` (ADR-0006); scaffold `tests/testthat/helper.R` with `skip_if_no_db()`; migrate the existing `test/test_crime_rate_regression.R` (self-documented "not CI") into the testhat structure under the skip convention. Detail lives in wayfinder ticket [#5](../../wayfinder/tickets/05-ci-design.md).
