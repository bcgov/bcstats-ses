# Config architecture & annual-refresh contract

- **Type:** `wayfinder:grilling`
- **Status:** resolved
- **Assignee:** resolved (work-through session)
- **Blocked by:** ~~#2 (config audit)~~ — resolved
- **Blocks:** —
- **Parent map:** [MAP](../MAP.md)

## Question

Decide the target `{config}` architecture: the split between `config_year.yml` (tracked) and `config.yml` (gitignored), what migrates where, and the precise annual-refresh contract ("edit config + re-run").

## Resolution

Both decisions resolved. See [ADR-0005](../../docs/adr/0005-config-architecture.md) for the durable record.

**D1 — Config split (audit-proposed 46/3/15):**
- `config_year.yml` (git-tracked) holds the **46** year-sensitive, non-secret refresh values: GCS snapshot table, year ranges (crime `2000→`, wildfire `2000→` + current-year boundary, pop `2000→`, CHSA ranges), catalogue `output_year`, all LAN file paths, download URLs (release-vintage), BC Data Catalogue record IDs, indigenous CSD type codes, census vintage.
- `config.yml` (gitignored) holds the **3** server-detail values — SQL catalog (`Population_Labour_Social`), schema (`Prod`), LAN base path (if environment-specific) — alongside existing secrets/credentials.
- **15** immutable constants stay inline (census-year labels in `01`, physical conversions, province code `"59"`, `"British Columbia"` filter).

**D2 — Refresh contract (sentinel + `validate_refresh()`):**
A `refresh_year` sentinel sits at the top of `config_year.yml`. A `validate_refresh()` helper runs at pipeline start and **fails fast** if any year-bearing value's embedded year disagrees with `refresh_year` — the GCS table's `YYYYMM`, `output_year`, the END years of ranges, and the release-vintage segment of download URLs. This is the mechanism that would have caught the `06b` drift ([#9](09-fix-06b-stale-gcs-snapshot.md)). The annual refresh becomes: **bump `refresh_year` + the year-bearing values it anchors → `validate_refresh()` → pipeline**.

**Follow-on (execution, not decisions):** migrate the 17 scripts to read `config_year.yml` (only `03`/`04` do today), add `validate_refresh()` to `R/utils.R`, document the contract in `ANNUAL_REFRESH.md`. The way is clear — this is build work.
