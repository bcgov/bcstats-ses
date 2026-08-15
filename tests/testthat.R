# testthat runner (ADR-0008). Run with testthat::test_local() from the
# project root. Pure-logic tests run everywhere; DB/LAN-coupled tests
# self-skip via skip_if_no_db() (helper.R) where there is no DB (CI).

library(testthat)

# Tests source R/config.R and R/transformations.R themselves via helper.R;
# testthat sets the working directory so relative sources resolve.
test_check("testthat")
