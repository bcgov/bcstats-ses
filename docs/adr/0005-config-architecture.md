# Config architecture: tracked split + refresh_year sentinel

The **46** year-sensitive, non-secret refresh values migrate into `config_year.yml` (git-tracked). `config.yml` (gitignored) keeps only secrets/credentials plus **3** server-detail values (SQL catalog `Population_Labour_Social`, schema `Prod`, LAN base path). **15** immutable constants (census-year labels, physical conversions, province code `"59"`) stay inline.

Each refresh is anchored by a `refresh_year` sentinel at the top of `config_year.yml`. A `validate_refresh()` helper runs at pipeline start and **fails fast** if any year-bearing value disagrees with `refresh_year` — the GCS table's embedded `YYYYMM`, `output_year`, range end-years, and the release-vintage segment of download URLs.

**Rationale:** the `06b` stale-snapshot bug — wildfire silently read `FCT_GCS_202509` while every other script used `FCT_GCS_202606` — showed that a manual annual checklist can't reliably catch year-value drift across 46 values. A sentinel plus an automated check turns that drift from a *silent* failure into a *run-start error*.

**Considered:**
- Documented checklist only — rejected: relies on the operator not skipping a value, which is exactly how `06b` slipped.
- Derive all year-values from `refresh_year` — rejected: irregular values like `FCT_GCS_202606` table names and StatsCan release URLs don't derive cleanly from a year; brittle if derivation rules change.

**Consequences:** scripts must read `config_year.yml` (only `03`/`04` do today — migration is build work); `validate_refresh()` lives in `R/utils.R`; the annual refresh is: bump `refresh_year` + its anchored values → `validate_refresh()` → pipeline. Detail lives in wayfinder ticket [#4](../../wayfinder/tickets/04-config-architecture.md).
