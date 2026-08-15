# parse_speed() — relocated from 14_connectivity.R (ADR-0009). Pure: no I/O.

test_that("parse_speed parses standard down_up tiers", {
  result <- parse_speed(c("50_10", "25_5", "10_2", "5_1"))
  expect_equal(result$down, c(50, 25, 10, 5))
  expect_equal(result$up, c(10, 5, 2, 1))
  expect_equal(result$is_lt5_1, rep(FALSE, 4))
})

test_that("parse_speed maps the <5_1 tier to zeros with flag", {
  result <- parse_speed("<5_1")
  expect_equal(result$down, 0)
  expect_equal(result$up, 0)
  expect_true(result$is_lt5_1)
})

test_that("parse_speed returns NA for unparseable and missing values", {
  result <- parse_speed(c(NA, "unknown", ""))
  expect_true(all(is.na(result$down)))
  expect_true(all(is.na(result$up)))
  expect_equal(result$is_lt5_1, rep(FALSE, 3))
})

test_that("parse_speed handles a mixed vector preserving order", {
  result <- parse_speed(c("<5_1", "50_10", NA))
  expect_equal(result$down, c(0, 50, NA))
  expect_equal(result$up, c(0, 10, NA))
  expect_equal(result$is_lt5_1, c(TRUE, FALSE, FALSE))
})
