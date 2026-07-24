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
- [#8 Secrets/reproducibility audit](tickets/08-secrets-reproducibility-audit.md) — clean bill: no secrets tracked (`Trusted_Connection=Yes` is non-secret Windows auth); `.Renviron` + `config.yml` gitignored; `config_year.yml` non-secret; `safepaths` used consistently. Minor: machine-specific cache paths in tracked config (defer to #4).

## Ticket index

| # | Title | Type | Status | Blocked by |
|---|-------|------|--------|-----------|
| 1 | [Testability audit](tickets/01-testability-audit.md) | research | **resolved** | — |
| 2 | [Config audit](tickets/02-config-audit.md) | research | open *(in-flight)* | — |
| 3 | [Testing strategy](tickets/03-testing-strategy.md) | grilling | open | ~~1~~ unblocked |
| 4 | [Config architecture & annual-refresh contract](tickets/04-config-architecture.md) | grilling | open | 2 |
| 5 | [CI design](tickets/05-ci-design.md) | grilling | open | ~~1~~ unblocked |
| 6 | [lint/format adoption](tickets/06-lint-format-adoption.md) | grilling | open | — |
| 7 | [Modularization scope](tickets/07-modularization-scope.md) | grilling | open | ~~1~~ unblocked |
| 8 | [Secrets/reproducibility audit](tickets/08-secrets-reproducibility-audit.md) | task | **resolved** | — |

**Frontier (open · unblocked · unclaimed):** #3, #5, #6, #7. (#2 research in-flight; #4 blocked by #2.)

## Not yet specified

- The earlier fog — *"how DB-coupled logic ever gets test coverage"* — has **graduated** into #3 (testing strategy), now that #1 established only ~10% of the pipeline is pure/testable. #3 will decide whether the remaining ~90% gets any coverage (mocks? LAN-only integration?) or is accepted as untested. No new fog yet.

## Out of scope

- **`{targets}` orchestration** — ruled out by the chosen depth (incremental practices only).
- **R-package-ification** (DESCRIPTION/NAMESPACE/R/, R CMD check).
- **Running the full pipeline in CI** — impossible without LAN/SQL Server access.
- **pkgdown** documentation site.
