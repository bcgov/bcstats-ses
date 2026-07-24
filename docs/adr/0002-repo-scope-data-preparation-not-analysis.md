# Repo scope: data preparation (clean → suppress → deliver), not analysis

This repo cleans external datasets and prepares catalogue- and downstream-ready outputs for the SES index. It does **not** compute the index, and it does **not** host exploratory analysis as part of the pipeline.

We accept that the repo has grown past pure cleaning, and **organize** the expanded scope into explicit stages (Cleaning, Suppression, Delivery) rather than trimming functional code. Exploratory work — the connectivity–SES correlation analysis and the demo Shiny apps — is moved out of the pipeline into `analysis/` and `apps/experimental/` and kept for reference only. `app.R` is the sole canonical Shiny entry point.

**Consequences**:
- Cleaned **data outputs (CSVs) are not tracked in git**; they are delivered via the BC Data Catalogue. `.gitignore` keeps `*.csv` ignored and `R/execution_log.txt` untracked.
- Future "delivery" scripts belong in the `17`+ numbering range; experimental apps stay under `apps/experimental/`.
