# Secrets / reproducibility audit

- **Type:** `wayfinder:task`
- **Status:** open
- **Assignee:** unclaimed
- **Blocked by:** —
- **Blocks:** —
- **Parent map:** [MAP](../MAP.md)

## Question

Confirm no secrets are tracked in this public repo:

- Scan code and config for credentials, keys, tokens, connection strings (e.g., `grep` for `password`, `uid`, `token`, `Driver=`, inline server names).
- Verify `.Renviron` and `config.yml` are gitignored and that secrets live there, not in `config_year.yml` or scripts.
- Verify `safepaths` usage is consistent (no raw LAN paths committed where `safepaths` should be used).

Output: a clean bill of health, or a prioritized remediation list. This is a **task** (manual check), not a decision — it earns its place by assuring the config (#4) and CI (#5) work is safe to build on. Record what was checked and any findings as the resolution.
