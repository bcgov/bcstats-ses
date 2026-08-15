# Shared test helpers (ADR-0007/0008).
#
## ----------------------------------------------------------
## Reasons for change, other notes
## ----------------------------------------------------------
## The pipeline depends on a secure LAN / SQL Server that CI does not
## have. DB/LAN-coupled tests self-skip via skip_if_no_db() — CI has no
## DB, so those tests skip automatically while pure-logic tests run.
## Gating knowledge lives in the TEST (which knows it needs a DB), not
## in the workflow (which would need a drift-prone path list).

# Skip helper for tests that require the secure LAN / SQL Server.
# Availability is opt-IN via an explicit env var set only in the secure
# environment (BCSTATS_DB_AVAILABLE=1). This deliberately does NOT probe
# for ODBC drivers — a dev laptop has drivers but no LAN/DB access, and a
# false "available" would try to run the full pipeline and error. CI and
# ordinary dev machines leave the var unset, so DB tests skip cleanly.
skip_if_no_db <- function() {
  if (!nzchar(Sys.getenv("BCSTATS_DB_AVAILABLE"))) {
    testthat::skip("No SQL Server / LAN access (set BCSTATS_DB_AVAILABLE=1 to run)")
  }
}

# Pure helpers, sourced relative to the project root (testthat::test_local()
# and R CMD check both run with the project root as the working directory).
for (f in c("R/config.R", "R/transformations.R")) {
  if (file.exists(f)) {
    source(f)
  } else {
    # Fall back to locating via this file (e.g. when run via R CMD check
    # from the tests directory).
    source(file.path("..", "..", f))
  }
}
