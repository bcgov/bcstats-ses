# validate_refresh() — the ADR-0005 drift gate (R/config.R). Pure.

test_that("validate_refresh passes when GCS year matches refresh_year", {
  yc <- list(refresh_year = 2026, gcs = list(table = "FCT_GCS_202606"))
  expect_length(validate_refresh(yc), 0)
})

test_that("validate_refresh catches a stale GCS snapshot (the 06b bug)", {
  yc <- list(refresh_year = 2026, gcs = list(table = "FCT_GCS_202509"))
  problems <- validate_refresh(yc)
  expect_length(problems, 1)
  expect_match(problems[1], "FCT_GCS_202509")
  expect_match(problems[1], "2025")
  expect_match(problems[1], "2026")
})

test_that("validate_refresh tolerates a missing refresh_year sentinel", {
  yc <- list(gcs = list(table = "FCT_GCS_202606"))
  expect_length(validate_refresh(yc), 0)
})

test_that("validate_refresh tolerates a missing gcs table", {
  yc <- list(refresh_year = 2026)
  expect_length(validate_refresh(yc), 0)
})

test_that("validate_refresh ignores tables without an embedded YYYYMM", {
  yc <- list(refresh_year = 2026, gcs = list(table = "NOT_A_TABLE"))
  expect_length(validate_refresh(yc), 0)
})
