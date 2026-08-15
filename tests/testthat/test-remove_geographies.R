# remove_geographies() — relocated from 15_remove_geo_suppression_ids.R
# (ADR-0009). Pure (operates on the passed data frame; cat() only).

test_that("remove_geographies drops the listed codes", {
  sei <- data.frame(
    GEO = c("111", "222", "333", "444"),
    VAL = 1:4,
    stringsAsFactors = FALSE
  )
  out <- remove_geographies(sei, "GEO", c("222", "444"), "CSD")
  expect_equal(out$GEO, c("111", "333"))
})

test_that("remove_geographies is a no-op when nothing matches", {
  sei <- data.frame(GEO = c("111", "222"), VAL = 1:2)
  out <- remove_geographies(sei, "GEO", c("999"), "CHSA")
  expect_equal(nrow(out), 2)
})

test_that("remove_geographies coerces factor/numeric code columns to character", {
  sei <- data.frame(GEO = factor(c("111", "222")), VAL = 1:2)
  out <- remove_geographies(sei, "GEO", c("222"), "CSD")
  expect_equal(out$GEO, "111")
  expect_type(out$GEO, "character")
})

test_that("remove_geographies removes everything when all codes match", {
  sei <- data.frame(GEO = c("111", "222"), VAL = 1:2)
  out <- remove_geographies(sei, "GEO", c("111", "222"), "CSD")
  expect_equal(nrow(out), 0)
})
