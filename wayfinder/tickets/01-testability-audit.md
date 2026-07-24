# Testability audit

- **Type:** `wayfinder:research`
- **Status:** open
- **Assignee:** unclaimed
- **Blocked by:** —
- **Blocks:** #3 (testing strategy), #5 (CI design), #7 (modularization scope)
- **Parent map:** [MAP](../MAP.md)

## Question

Classify each script in `src/` (`01`–`18`) by how much of its logic is **pure/testable** (data transforms, calculations, helpers — no I/O) vs. **coupled to the secure LAN/SQL Server** (DB queries, `safepaths`/LAN file I/O, `bcdata` downloads).

Output a per-script table: testable functions/sections vs. DB/LAN-coupled sections, with a rough estimate of what fraction of the pipeline could be covered by unit tests run in CI (where there's no LAN/DB access). Note the existing `test/test_crime_rate_regression.R` snapshot test and how it's structured.

This unblocks the testing strategy (#3), CI design (#5), and modularization scope (#7). Capture findings in `wayfinder/research/01-testability-audit.md` and link back here.
