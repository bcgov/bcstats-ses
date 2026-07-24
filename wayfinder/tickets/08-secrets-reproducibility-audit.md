# Secrets / reproducibility audit

- **Type:** `wayfinder:task`
- **Status:** resolved
- **Assignee:** resolved (AFK)
- **Blocked by:** —
- **Blocks:** —
- **Parent map:** [MAP](../MAP.md)

## Question

Confirm no secrets are tracked in this public repo; verify `.Renviron`/`config.yml` are gitignored and `safepaths` is used consistently.

## Resolution

**Clean bill of health on secrets.**

1. **No credential/secret patterns in tracked files.** The `Trusted_Connection = "Yes"` literals in `03`/`04`/`15` and the `config$database$trusted_connection` references in `06b`/`10`/`12` are **Windows-auth settings, not secrets** (no password/credential). Strong-indicator grep (`password`, `token`, `api_key`, `Driver=`, `Server=`, etc.) returned nothing sensitive.
2. **Secrets are gitignored correctly:** `.Renviron` (`.gitignore:29`) and `config.yml` (`.gitignore:36`) are both ignored; secrets belong there.
3. **`config_year.yml` is explicitly non-secret** (GCS snapshot table + cache paths), as its header comment states.
4. **`safepaths` used consistently** — 16 calls across `src/`; **zero** raw drive-letter paths in code (all LAN access goes through `safepaths` or `config`).

**Minor non-secret portability note for #4:** the `cansim`/`cancensus` cache paths in tracked `config_year.yml` (`C:/Temp/...`) are machine-specific; better placed in per-user `.Renviron` so they don't assume one developer's machine. Not a secret — a config-discipline item.
