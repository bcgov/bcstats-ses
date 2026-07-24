# Data vintage caps at 2024; branches named by purpose, not data year

Source data for the SES index only extends to **2024**. The "2026 update" is therefore a **2026-period release and finalization of the 2024-data SEI**, not a refresh to newer data. The `FCT_GCS_202606` snapshot referenced in `config_year.yml` is a June-2026 geography-reference snapshot of the Translation Master File — not 2026 SES data.

Because the work year (2026) and the data year (2024) differ, branches are named by **purpose** rather than by year, to avoid the drift seen in `sei_2024_data` — a 2024-data branch carrying a 2026-dated config. The release branch is `sei_2024_release`.

**Consequences**: A snapshot bump must never be read as "newer data." When 2025 source data arrives it will warrant a genuinely new data-year effort, at which point this ADR should be revisited.
