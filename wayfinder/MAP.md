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

(none yet)

## Ticket index

| # | Title | Type | Status | Blocked by |
|---|-------|------|--------|-----------|
| 1 | [Testability audit](tickets/01-testability-audit.md) | research | open | — |
| 2 | [Config audit](tickets/02-config-audit.md) | research | open | — |
| 3 | [Testing strategy](tickets/03-testing-strategy.md) | grilling | open | 1 |
| 4 | [Config architecture & annual-refresh contract](tickets/04-config-architecture.md) | grilling | open | 2 |
| 5 | [CI design](tickets/05-ci-design.md) | grilling | open | 1 |
| 6 | [lint/format adoption](tickets/06-lint-format-adoption.md) | grilling | open | — |
| 7 | [Modularization scope](tickets/07-modularization-scope.md) | grilling | open | 1 |
| 8 | [Secrets/reproducibility audit](tickets/08-secrets-reproducibility-audit.md) | task | open | — |

**Frontier (open · unblocked · unclaimed):** 1, 2, 6, 8.

## Not yet specified

- **How DB-coupled logic ever gets test coverage.** Can't ticket sharply until the testability audit (#1) reports how much of the pipeline is pure vs. DB/LAN-bound. May graduate into a real testing approach, or dissolve entirely (if little is testable). Fog, toward the destination — not a scope boundary.

## Out of scope

- **`{targets}` orchestration** — ruled out by the chosen depth (incremental practices only).
- **R-package-ification** (DESCRIPTION/NAMESPACE/R/, R CMD check).
- **Running the full pipeline in CI** — impossible without LAN/SQL Server access.
- **pkgdown** documentation site.
