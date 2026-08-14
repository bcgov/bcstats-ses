# Shared infrastructure helpers for the SES Index data pipeline.
#
# Sourced by src/ scripts and by tests/testthat/ (see wayfinder ADR-0009).
# Contents:
#   - load_year_config() / validate_refresh()  : config_year.yml + drift check
#   - connect_db()                             : normalized DB connection
# Pure, unit-testable transformations live in R/transformations.R.

## ----------------------------------------------------------
## Year-sensitive refresh configuration (ADR-0005)
## ----------------------------------------------------------
## config_year.yml is git-tracked and holds the year-sensitive, non-secret
## refresh parameters. load_year_config() is the single loader used by all
## scripts; it runs validate_refresh() so a year-value drift fails fast at
## run start instead of silently producing wrong outputs (the 06b lesson:
## wayfinder #9 found wildfire reading FCT_GCS_202509 while the pipeline
## standard was FCT_GCS_202606).

#' Load year-sensitive refresh parameters from config_year.yml
#'
#' Reads config_year.yml (git-tracked) and validates that year-bearing
#' values agree with the refresh_year sentinel (when present).
#'
#' @param config_file Path to the year config file; default "config_year.yml"
#'   resolved from the current working directory (scripts run from project root).
#' @return The parsed config list (invisibly); errors on validation failure.
load_year_config <- function(config_file = "config_year.yml") {
  year_config <- config::get(file = config_file)

  problems <- validate_refresh(year_config)
  if (length(problems) > 0) {
    stop(
      "config_year.yml refresh validation failed:\n  - ",
      paste(problems, collapse = "\n  - "),
      "\nFix config_year.yml (or its refresh_year sentinel) before re-running.",
      call. = FALSE
    )
  }

  invisible(year_config)
}

#' Validate year-bearing values against the refresh_year sentinel
#'
#' Checks that values whose content embeds a year are consistent with
#' refresh_year. Currently validates:
#'   - gcs$table : the GCS snapshot table embeds YYYYMM; its year must
#'                 match refresh_year (e.g. FCT_GCS_202606 <-> refresh_year 2026).
#'
#' As more year-bearing values migrate into config_year.yml (wayfinder #4),
#' add their checks here — this function is the single drift gate.
#'
#' @param year_config The parsed config_year.yml list.
#' @return Character vector of problem descriptions; empty when consistent.
validate_refresh <- function(year_config) {
  problems <- character(0)
  refresh_year <- year_config$refresh_year

  gcs_table <- year_config$gcs$table
  if (!is.null(gcs_table) && !is.na(gcs_table)) {
    m <- regmatches(gcs_table, regexpr("[0-9]{6}", gcs_table))
    if (length(m) > 0 && nchar(m) > 0) {
      table_year <- substr(m, 1, 4)
      if (!is.null(refresh_year) && !is.na(refresh_year) &&
        table_year != as.character(refresh_year)) {
        problems <- c(
          problems,
          sprintf(
            "gcs$table '%s' embeds year %s but refresh_year is %s",
            gcs_table, table_year, refresh_year
          )
        )
      }
    }
  }

  problems
}
