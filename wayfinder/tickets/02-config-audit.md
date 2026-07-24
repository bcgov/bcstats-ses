# Config audit

- **Type:** `wayfinder:research`
- **Status:** open
- **Assignee:** unclaimed
- **Blocked by:** —
- **Blocks:** #4 (config architecture & annual-refresh contract)
- **Parent map:** [MAP](../MAP.md)

## Question

Find every hardcoded **year, file path, table name, and magic value** across `src/` that should be configuration. For each, record where it currently lives (inline literal in which script/line) and where it should live: `config_year.yml` (tracked — year-sensitive, non-secret refresh values like the GCS snapshot table) or `config.yml` (gitignored — secrets, credentials, connection details).

Note what `config_year.yml` and the `{config}` package already provide (some scripts already read `config_year.yml`), so the inventory reflects the current state, not a greenfield. Output: an inventory with a proposed config home for each value.

This unblocks config architecture (#4). Capture findings in `wayfinder/research/02-config-audit.md` and link back here.
