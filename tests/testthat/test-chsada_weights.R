# compute_chsada_weights() — extracted from 12_output_CHSA_DA_lookup.R
# (ADR-0009). Pure: nested population totals + allocation weights.

library(dplyr)

# Minimal fixture: 2 years, 2 CHSAs, 3 DAs, DB-level rows.
# DBs aggregate to CHSADA (CHSA x DA) units; weights derive from the
# nested population sums. Values chosen so ratios are exact.
make_db_pop <- function() {
  bind_rows(
    # YEAR 1: CHSA A splits DA 1 into 20% / 80% by population
    data.frame(YEAR = 1, CHSA = "A", DAUID = "01", POPULATION = 20),
    data.frame(YEAR = 1, CHSA = "A", DAUID = "01", POPULATION = 80),
    # DA 1 fully inside CHSA A via a second DB
    data.frame(YEAR = 1, CHSA = "A", DAUID = "01", POPULATION = 100),
    # DA 2 fully in CHSA B
    data.frame(YEAR = 1, CHSA = "B", DAUID = "02", POPULATION = 50),
    # YEAR 2 repeats the same shape
    data.frame(YEAR = 2, CHSA = "A", DAUID = "01", POPULATION = 20),
    data.frame(YEAR = 2, CHSA = "A", DAUID = "01", POPULATION = 80),
    data.frame(YEAR = 2, CHSA = "A", DAUID = "01", POPULATION = 100),
    data.frame(YEAR = 2, CHSA = "B", DAUID = "02", POPULATION = 50)
  )
}

test_that("compute_chsada_weights computes nested population totals", {
  w <- compute_chsada_weights(make_db_pop())
  year1 <- w |> filter(YEAR == 1, CHSA == "A", DAUID == "01")
  # All three DBs collapse into ONE CHSADA row: 20+80+100 = 200
  expect_equal(nrow(year1), 1)
  expect_equal(year1$chsada_pop, 200)
  expect_equal(year1$da_pop, 200)      # DA 01 total = 200 (all in CHSA A)
  expect_equal(year1$chsa_pop, 200)    # CHSA A total = 200
})

test_that("compute_chsada_weights derives the two allocation weights", {
  w <- compute_chsada_weights(make_db_pop())
  year1 <- w |> filter(YEAR == 1, CHSA == "A", DAUID == "01")
  # DA fully inside one CHSA: both weights are exactly 1
  expect_equal(year1$chsada_to_da_pop_ratio, 1)
  expect_equal(year1$chsada_to_chsa_pop_ratio, 1)
})

test_that("compute_chsada_weights splits a DA across two CHSAs", {
  db_pop <- bind_rows(
    # DA 1 split 60/40 across CHSA A and B
    data.frame(YEAR = 1, CHSA = "A", DAUID = "01", POPULATION = 60),
    data.frame(YEAR = 1, CHSA = "B", DAUID = "01", POPULATION = 40),
    # CHSA B also fully covers DA 2
    data.frame(YEAR = 1, CHSA = "B", DAUID = "02", POPULATION = 60)
  )
  w <- compute_chsada_weights(db_pop)

  da1_a <- w |> filter(CHSA == "A", DAUID == "01")
  da1_b <- w |> filter(CHSA == "B", DAUID == "01")
  expect_equal(da1_a$chsada_pop, 60)
  expect_equal(da1_b$chsada_pop, 40)
  # DA 1 total = 100 -> 60/100 and 40/100
  expect_equal(da1_a$chsada_to_da_pop_ratio, 0.6)
  expect_equal(da1_b$chsada_to_da_pop_ratio, 0.4)
  # CHSA B total = 100 (40 from DA1 + 60 from DA2) -> 40/100
  expect_equal(da1_b$chsada_to_chsa_pop_ratio, 0.4)
  # CHSA A total = 60 -> 60/60
  expect_equal(da1_a$chsada_to_chsa_pop_ratio, 1)
})

test_that("compute_chsada_weights builds chsada_id and keeps years separate", {
  w <- compute_chsada_weights(make_db_pop())
  expect_true(all(grepl("^[AB][0-9]{2}$", w$chsada_id)))
  expect_equal(nrow(w |> filter(YEAR == 1)), nrow(w |> filter(YEAR == 2)))
  expect_equal(nrow(w), 4) # 2 per year
})
