# Config audit

- **Type:** `wayfinder:research`
- **Status:** resolved (by /research subagent)
- **Assignee:** resolved
- **Blocked by:** —
- **Blocks:** #4 (config architecture & annual-refresh contract) — now unblocked
- **Parent map:** [MAP](../MAP.md)

## Question

Find every hardcoded year, file path, table name, and magic value across `src/` that should be configuration; propose a config home for each. Note what `config_year.yml`/`{config}` already provide.

## Resolution

Resolved by /research subagent; full report at [`../research/02-config-audit.md`](../research/02-config-audit.md).

- **64 hardcoded values across 17 scripts:** 46 should migrate to `config_year.yml`, 3 to `config.yml`, 15 are fine inline.
- **Config adoption is shallow:** only scripts `03` and `04` currently read `config_year.yml`; the other 15 use `config.yml` or nothing.
- **Biggest offenders:** `09_output_remoteness.R` (8+ LAN paths, 2 URLs, zero `config_year` use), `17_data_preparation_for_powerbi.R` (6+ dated LAN paths).
- **CRITICAL (correctness):** `06b_output_wildfire.R:130` reads a **stale GCS snapshot `FCT_GCS_202509`**, bypassing `config_year.yml`'s `FCT_GCS_202606` — wildfire data is resolved against an older geography snapshot than the rest of the pipeline. Spun off as **#9**.
- **No secrets exposed** — corroborates #8; all credentials/API keys live in the gitignored `config.yml`.

**Unblocks:** #4 (config architecture & annual-refresh contract).
