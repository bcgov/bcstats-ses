# Refactor scope: incremental practices, not re-architecture

This repo's "best data-science and software-engineering practice" refactor will be **incremental practices layered on the existing numbered-script pipeline** — not a `{targets}` re-architecture or R-package-ification.

**Driver:** regression safety and maintainability for annual refreshes (each year's run should be a config bump + re-run, and changes shouldn't silently break outputs), not onboarding or audit-readiness.

**Considered options:**
- `{targets}` orchestration + practices — rejected for this effort: highest-leverage R practice, but a re-architecture beyond the agreed scope.
- Full R-package-ification (DESCRIPTION/NAMESPACE/R/, R CMD check) — rejected: the repo is a pipeline, not a reusable library; overkill.
- Incremental practices only — **chosen**: lowest risk, preserves a working production pipeline, fits the secure-environment constraint (the DB-dependent pipeline can't run in CI anyway).

**Consequences:** `{targets}`, package-ification, full-pipeline-in-CI, and pkgdown are out of scope. The work focuses on tests, config discipline, CI guardrails for testable logic, targeted modularization, and reproducibility/secrets hygiene. If a later year's refresh reveals the numbered-script model can't be kept safe, this ADR is revisited and a re-architecture effort opened fresh.
