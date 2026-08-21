# Lint/format adoption: restyle-on-touch, styler gate + lintr advisory

Adopt `{lintr}` + `{styler}` with a **restyle-on-touch** policy: CI gates only the files changed in a PR, not the whole codebase. On those changed files, **`styler` is a blocking gate** (dry-run must come back clean) and **`lintr` is advisory** (reports findings, doesn't block).

**Rationale:** the codebase has never been styled — 8,187 lines across 17 scripts, no prior `styler` pass, no `.lintr`. The driver is regression *safety*, not "clean code" for its own sake, so what matters is that **new** changes (annual-refresh edits) are clean, not that all historical lines are scrubbed today. A fix-first pass would produce an 8k-line diff that obscures real history and a manual lint-triage backlog — churn that doesn't buy regression safety. Restyle-on-touch converges naturally: each file is cleaned when next touched.

A full-default `lintr` gate on touched files would fail on each file's pre-existing naming/line-length debt — too much friction. So `styler` (cosmetic, no triage) blocks, while `lintr` surfaces the backlog without blocking until it's understood and triaged.

**Considered:**
- Fix-first + strict gate — rejected: one-time 8k-line churn + manual triage; doesn't serve the driver.
- Advisory only (no CI gate) — rejected: no enforcement; theater that doesn't serve regression safety.

**Consequences:** add `lintr` + `styler` to `renv.lock`; add a minimal `.lintr` (defaults, tightenable later); implement the lint/style CI job (gating changed files) as part of [#5 (CI design)](../../wayfinder/tickets/05-ci-design.md). Detail lives in wayfinder ticket [#6](../../wayfinder/tickets/06-lint-format-adoption.md).
