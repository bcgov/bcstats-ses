# compute_combined_max() — relocated from 14_connectivity.R (ADR-0009). Pure.

test_that("compute_combined_max returns the higher tier", {
  expect_equal(compute_combined_max("10_2", "50_10"), "50_10")
  expect_equal(compute_combined_max("50_10", "10_2"), "50_10")
  expect_equal(compute_combined_max("5_1", "25_5"), "25_5")
})

test_that("compute_combined_max handles equal tiers (wired wins ties)", {
  expect_equal(compute_combined_max("50_10", "50_10"), "50_10")
  expect_equal(compute_combined_max("5_1", "5_1"), "5_1")
})

test_that("compute_combined_max returns NA only when both are NA/empty", {
  expect_true(is.na(compute_combined_max(NA, NA)))
  expect_true(is.na(compute_combined_max("", "")))
  expect_equal(compute_combined_max(NA, "25_5"), "25_5")
  expect_equal(compute_combined_max("25_5", NA), "25_5")
  expect_equal(compute_combined_max("", "10_2"), "10_2")
})

test_that("compute_combined_max ranks the full tier hierarchy", {
  # ascending: <5_1 < 5_1 < 10_2 < 25_5 < 50_10
  expect_equal(compute_combined_max("<5_1", "5_1"), "5_1")
  expect_equal(compute_combined_max("10_2", "25_5"), "25_5")
  expect_equal(compute_combined_max("10_2", "<5_1"), "10_2")
})
