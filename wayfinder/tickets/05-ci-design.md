# CI design

- **Type:** `wayfinder:grilling`
- **Status:** open
- **Assignee:** unclaimed
- **Blocked by:** #1 (testability audit)
- **Blocks:** —
- **Parent map:** [MAP](../MAP.md)

## Question

Design the GitHub Actions CI that runs on every PR:

- What runs: `{lintr}`, `{styler}` check, R syntax check, and the non-DB tests (per #1 / #3).
- R version matrix (single version vs. range), and `renv::restore()` with cache.
- How DB/LAN-dependent tests and scripts are **tagged or gated** so CI skips them gracefully (e.g., `skip_if_no_db()` helper, `testthat::skip_on_ci()`, or a profile).
- Explicitly: CI guards **code quality + testable logic only**, never end-to-end pipeline runs (no LAN/SQL Server).

Resolve via `/grilling` + `/domain-modeling`; record the answer as a resolution comment, then close and add a one-line gist to the map's Decisions so far.
