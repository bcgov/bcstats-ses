# Regression test for the crime rate pipeline (src/03_output_crime_rate.R).
#
## ----------------------------------------------------------
## Reasons for change, other notes
## ----------------------------------------------------------
## Migrated from test/test_crime_rate_regression.R into tests/testthat/
## (ADR-0008 Tier 2): DB/LAN-coupled, so it self-skips in CI via
## skip_if_no_db() and runs locally on the secure LAN.
##
## Purpose: prove that parameterizing the pipeline (config_year.yml reads,
## crime_start_year, dynamic dictionary end year) is BEHAVIOR-PRESERVING
## by comparing the produced DA-level crime rate output against a golden
## snapshot.
##
## Record/replay: if no snapshot exists at tests/snapshots/, the test runs
## the pipeline once and SAVES its output as the baseline (commit it);
## thereafter it compares equal dims, identical columns, value equality.
## NOTE: snapshots live under tests/ (NOT test/) because .gitignore ignores
## test/ — baselines must be committable.
## Because the upstream cansim data refreshes annually, the comparison is
## most meaningful in the SAME data window the snapshot was recorded.

test_that("crime rate pipeline output matches golden snapshot", {
  skip_if_no_db()

  project_root <- here::here()
  snapshot_dir <- file.path(project_root, "tests", "snapshots")
  snapshot_file <- file.path(snapshot_dir, "BC_DA_Crime_Rate_golden.csv")
  out_dir <- file.path(project_root, "out")

  run_pipeline <- function() {
    source(file.path(project_root, "src", "03_output_crime_rate.R"),
      echo = FALSE, local = FALSE
    )
  }

  read_output <- function() {
    cands <- list.files(
      out_dir,
      pattern = "^BC_DA_Crime_Rate_DIP.*\\.csv$",
      full.names = TRUE
    )
    if (length(cands) == 0) {
      stop("No output CSV found in out/ after running the pipeline.")
    }
    out_file <- cands[order(file.info(cands)$mtime, decreasing = TRUE)][1]
    readr::read_csv2(out_file, show_col_types = FALSE)
  }

  if (!dir.exists(snapshot_dir)) dir.create(snapshot_dir, recursive = TRUE)

  if (!file.exists(snapshot_file)) {
    # ---------------- RECORD MODE ----------------
    run_pipeline()
    current <- read_output()
    readr::write_excel_csv2(current, snapshot_file)
    message("RECORDED baseline: ", snapshot_file,
      " (", nrow(current), " rows). Review and commit, then re-run.")
  } else {
    # ---------------- REPLAY MODE ----------------
    run_pipeline()
    current <- read_output()
    baseline <- readr::read_csv2(snapshot_file, show_col_types = FALSE)
    expect_identical(dim(current), dim(baseline))
    expect_identical(names(current), names(baseline))
    expect_true(isTRUE(all.equal(current, baseline)))
  }
})
