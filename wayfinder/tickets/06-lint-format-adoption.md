# lint/format adoption

- **Type:** `wayfinder:grilling`
- **Status:** resolved
- **Assignee:** resolved (work-through session)
- **Blocked by:** —
- **Blocks:** —
- **Parent map:** [MAP](../MAP.md)

## Question

Decide `{lintr}` / `{styler}` adoption: fix-first vs incremental, gate vs advisory, and the style/lint baseline. Note: the codebase is not currently lint/style-clean.

## Resolution

Both decisions resolved. See [ADR-0006](../../docs/adr/0006-lint-format-adoption.md).

**D1 — Restyle-on-touch:** CI gates only the files changed in a PR (the diff); untouched code stays as-is and converges naturally as files are edited during annual refreshes. No one-time 8k-line restyle diff, no manual lint-triage backlog — the driver is regression *safety*, so what matters is that **new** changes are clean, not that all 8,187 historical lines are scrubbed today.

**D2 — Styler gate + lintr advisory (on changed files):**
- **`styler` gate (blocking):** changed files must come back from `styler::style_file(dry = "fail")` clean. Enforces formatting consistency on new edits — the visible win, no triage.
- **`lintr` advisory (non-blocking):** CI runs `lintr::lint()` on changed files and reports findings, but doesn't block the PR. Surfaces the backlog (unused vars, `=` vs `<-`, partial args) for awareness; can be graduated to a gate once the backlog is triaged.

**Prereq / setup (build work):**
- Add `lintr` + `styler` to `renv.lock` (`renv::install("lintr","styler")`).
- Add a minimal `.lintr` config (start with sensible defaults; can tighten later).
- Add the lint/style job to the CI workflow defined in [#5 (CI design)](05-ci-design.md) — gating only changed files (e.g. via `git diff` to enumerate `*.R`/`*.r` in the PR).
