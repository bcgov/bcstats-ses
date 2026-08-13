# Wayfinder Map: Regression-safe, annually-refreshable pipeline

> **Local-markdown tracker** (no `gh`; repo is public). The map is the canonical artifact; tickets live in `tickets/`. "Decisions so far" records closed tickets only; the **Ticket index** below substitutes for the query a real issue UI would provide. Work tickets **one per session** (claim by setting `Assignee`); research tickets are the exception and may be resolved in the charting session.

## Destination

A regression-safe, annually-refreshable data-cleaning pipeline: the existing numbered-script structure, hardened so that (a) changes can't silently break outputs and (b) each year's refresh is a config bump + re-run, not a re-investigation. **Incremental SWE practices only — no re-architecture.** See [ADR-0004](../docs/adr/0004-refactor-scope-incremental-not-rearchitecture.md).

## Notes

- **Domain:** BC community SES index data preparation. See [`../CONTEXT.md`](../CONTEXT.md) for the glossary and [`../docs/adr/`](../docs/adr/) for prior decisions.
- **Driver:** regression safety + maintainability for annual refreshes.
- **Skills every session should consult:** `/grilling` + `/domain-modeling` for decision tickets; `/research` for research tickets; `/prototype` if a concrete artifact would clarify a decision.
- **Standing constraints:** the pipeline depends on a secure LAN/SQL Server — it **cannot** run end-to-end in CI. The repo is **public** — never commit secrets or publish internal artifacts without explicit confirmation. Follow the no-AI-attribution rule (`~/.claude/rules/git-no-ai-attribution.md`).

## Decisions so far

<!-- one line per closed ticket: gist + link -->

- [#1 Testability audit](tickets/01-testability-audit.md) — ~10% of pipeline logic is CI-testable (it's I/O-bound); existing crime-rate test is a local-only snapshot, not testthat; top unit-test candidates are `parse_speed`/`compute_combined_max` (`14_connectivity.R`) and `remove_geographies` (`15_…`); `utils.R` has zero testable functions.
- [#2 Config audit](tickets/02-config-audit.md) — 64 hardcoded values across 17 scripts (46 → `config_year.yml`, 3 → `config.yml`, 15 inline); only `03`/`04` read `config_year.yml` today; **critical:** `06b` used a stale GCS snapshot (`FCT_GCS_202509` vs configured `FCT_GCS_202606`) → fixed in #9; no secrets exposed.
- [#4 Config architecture & annual-refresh contract](tickets/04-config-architecture.md) — split 46 tracked (`config_year.yml`) / 3 server-detail (`config.yml`) / 15 inline; refresh contract = `refresh_year` sentinel + `validate_refresh()` that fails fast on year-value drift (would've caught the 06b bug). See [ADR-0005](../docs/adr/0005-config-architecture.md).
- [#5 CI design](tickets/05-ci-design.md) — R matrix 4.5.2 (lock) + 4.6.1 (dev); DB/LAN tests self-skip via `skip_if_no_db()`; lint/format gate on changed files (per #6); full pipeline never runs in CI. See [ADR-0007](../docs/adr/0007-ci-design.md).
- [#6 lint/format adoption](tickets/06-lint-format-adoption.md) — restyle-on-touch: CI gates only PR-changed files; `styler` blocks (dry-run), `lintr` advisory. No 8k-line churn. See [ADR-0006](../docs/adr/0006-lint-format-adoption.md).
- [#8 Secrets/reproducibility audit](tickets/08-secrets-reproducibility-audit.md) — clean bill: no secrets tracked (`Trusted_Connection=Yes` is non-secret Windows auth); `.Renviron` + `config.yml` gitignored; `config_year.yml` non-secret; `safepaths` used consistently. Minor: machine-specific cache paths in tracked config (defer to #4).
- [#9 Fix 06b stale GCS snapshot](tickets/09-fix-06b-stale-gcs-snapshot.md) — confirmed leftover bug; `06b` now reads the GCS table from `config_year.yml` via `sprintf` (was hardcoded `FCT_GCS_202509`); R parse clean. ⚠️ **Data impact:** re-run `06b` to regenerate wildfire outputs against `FCT_GCS_202606`; fix currently on the wayfinder branch.

## Ticket index

| # | Title | Type | Status | Blocked by |
|---|-------|------|--------|-----------|
| 1 | [Testability audit](tickets/01-testability-audit.md) | research | **resolved** | — |
| 2 | [Config audit](tickets/02-config-audit.md) | research | **resolved** | — |
| 3 | [Testing strategy](tickets/03-testing-strategy.md) | grilling | open | — |
| 4 | [Config architecture & annual-refresh contract](tickets/04-config-architecture.md) | grilling | **resolved** | — |
| 5 | [CI design](tickets/05-ci-design.md) | grilling | **resolved** | — |
| 6 | [lint/format adoption](tickets/06-lint-format-adoption.md) | grilling | **resolved** | — |
| 7 | [Modularization scope](tickets/07-modularization-scope.md) | grilling | open | — |
| 8 | [Secrets/reproducibility audit](tickets/08-secrets-reproducibility-audit.md) | task | **resolved** | — |
| 9 | [Fix 06b stale GCS snapshot](tickets/09-fix-06b-stale-gcs-snapshot.md) | task | **resolved** | — |

**Frontier (open · unblocked · unclaimed):** #3, #7.

## Not yet specified

- The earlier fog — *"how DB-coupled logic ever gets test coverage"* — **graduated** into #3 (testing strategy) once #1 established only ~10% is pure/testable. #3 will decide whether the remaining ~90% gets coverage (mocks? LAN-only integration?) or is accepted as untested. No new fog yet.

## Out of scope

- **`{targets}` orchestration** — ruled out by the chosen depth (incremental practices only).
- **R-package-ification** (DESCRIPTION/NAMESPACE/R/, R CMD check).
- **Running the full pipeline in CI** — impossible without LAN/SQL Server access.
- **pkgdown** documentation site.
