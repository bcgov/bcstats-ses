# Regression test for the crime rate pipeline (src/03_output_crime_rate.R).
#
# Purpose:
#   Prove that parameterizing the pipeline (config_year.yml reads, crime_start_year,
#   dynamic dictionary end year) is BEHAVIOR-PRESERVING by comparing the produced
#   DA-level crime rate output against a golden snapshot.
#
# Record/replay pattern:
#   - If no golden snapshot exists yet (test/snapshots/BC_DA_Crime_Rate_golden.csv),
#     the test runs the pipeline once and SAVES its output as the new baseline.
#     Commit that baseline to git, then subsequent runs compare against it.
#   - If a baseline exists, the test compares the freshly produced output to it:
#       equal row count, identical column set/order, and value-level equality.
#
# Requirements:
#   - Live access to the cansim table (35-10-0184-01) and the SQL Server database.
#   - Run FROM THE PROJECT ROOT so the pipeline's relative paths resolve, e.g.:
#       Rscript --vanilla test/test_crime_rate_regression.R
#   - This is a LOCAL, manually-run fidelity check (not CI). Because the upstream
#     cansim data refreshes annually, the comparison is most meaningful when run
#     in the SAME data window in which the golden snapshot was recorded.

suppressWarnings({
  library(here)
  library(readr)
})

project_root <- here::here()
snapshot_dir <- file.path(project_root, "test", "snapshots")
snapshot_file <- file.path(snapshot_dir, "BC_DA_Crime_Rate_golden.csv")

# The pipeline writes a year-suffixed CSV to out/; read_output() locates the
# most recently written match by pattern when comparing.
out_dir <- file.path(project_root, "out")

run_pipeline <- function() {
  cat("Running src/03_output_crime_rate.R...\n")
  # Source the full pipeline (it writes its output to out/).
  source(file.path(project_root, "src", "03_output_crime_rate.R"), echo = FALSE)
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
  # Most recently written file.
  out_file <- cands[order(file.info(cands)$mtime, decreasing = TRUE)][1]
  cat("Comparing against output:", out_file, "\n")
  readr::read_csv2(out_file, show_col_types = FALSE)
}

compare_frames <- function(current, baseline) {
  ok <- TRUE
  if (!identical(dim(current), dim(baseline))) {
    ok <- FALSE
    cat("FAIL: dimensions differ. current:",
        paste(dim(current), collapse = "x"),
        " baseline:", paste(dim(baseline), collapse = "x"), "\n")
  }
  if (!identical(names(current), names(baseline))) {
    ok <- FALSE
    cat("FAIL: column names/order differ.\n")
    cat("  current :", paste(names(current), collapse = ", "), "\n")
    cat("  baseline:", paste(names(baseline), collapse = ", "), "\n")
  }
  if (ok) {
    value_cmp <- all.equal(current, baseline)
    if (!isTRUE(value_cmp)) {
      ok <- FALSE
      cat("FAIL: value-level differences:\n")
      print(head(as.character(value_cmp), 20))
    }
  }
  ok
}

if (!dir.exists(snapshot_dir)) {
  dir.create(snapshot_dir, recursive = TRUE)
}

if (!file.exists(snapshot_file)) {
  # ---------------- RECORD MODE ----------------
  cat("No golden snapshot found. RECORDING a new baseline.\n")
  # Always run a fresh pipeline so the baseline reflects the current code,
  # not a possibly-stale CSV left over in out/.
  run_pipeline()
  current <- read_output()
  readr::write_excel_csv2(current, snapshot_file)
  cat("RECORDED baseline:", snapshot_file, "\n")
  cat("Rows:", nrow(current), " Cols:", ncol(current), "\n")
  cat("Review and commit this snapshot, then re-run to verify fidelity.\n")
} else {
  # ---------------- REPLAY MODE ----------------
  cat("Golden snapshot found. Running pipeline and comparing...\n")
  run_pipeline()
  current <- read_output()
  baseline <- readr::read_csv2(snapshot_file, show_col_types = FALSE)
  if (compare_frames(current, baseline)) {
    cat("PASS: output matches the golden snapshot",
        "(rows:", nrow(current), "cols:", ncol(current), ").\n")
    quit(status = 0)
  } else {
    cat("FAIL: output does NOT match the golden snapshot.\n")
    quit(status = 1)
  }
}
