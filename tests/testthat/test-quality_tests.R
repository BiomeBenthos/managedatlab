# Tests for quality_tests functions
# sf_predicates
# concat
# uniqueness

# Import data
dat <- readRDS(testthat::test_path("fixtures", "small_dataset.rds"))


#---------- MISSING VALUES ----------#
# Extra parameters for function
extra_pars <- list(na.strings = c("NA", "", NA),
                   cols = "missing")

# Result
res1 <- missing_vals(dat$missing, extra_pars)

# Expected results
expected1 <- data.frame(
  rows = c(5, 10),
  test_name = test_name["missing_vals"],
  err_message = err_message("missing_vals", extra_pars),
  row.names = NULL
)


#---------- VALUES INSIDE INTERVAL ----------#
# Extra parameters for function
extra_pars1 <- list(min_vals = 1,
                    max_vals = 10,
                    cols = "integ")
extra_pars2 <- list(min_vals = 3,
                    max_vals = 10,
                    cols = "integ")
extra_pars3 <- list(min_vals = 1,
                    max_vals = 8,
                    cols = "integ")
extra_pars4 <- list(min_vals = 30,
                    max_vals = 60,
                    cols = "num")
extra_pars5 <- list(min_vals = 40,
                    max_vals = 60,
                    cols = "num")
extra_pars6 <- list(min_vals = 30,
                    max_vals = 45,
                    cols = "num")


# Results
res1 <- vals_interval(dat$integ, extra_pars1)
res2 <- vals_interval(dat$integ, extra_pars2)
res3 <- vals_interval(dat$integ, extra_pars3)
res4 <- vals_interval(dat$num, extra_pars4)
res5 <- vals_interval(dat$num, extra_pars5)
res6 <- vals_interval(dat$num, extra_pars6)

# Expected results
expected1 <- NULL
expected2 <- data.frame(
  rows = c(1,2),
  test_name = test_name["vals_interval"],
  err_message = err_message("vals_interval",extra_pars2),
  row.names = NULL
)
expected3 <- data.frame(
  rows = c(9,10),
  test_name = test_name["vals_interval"],
  err_message = err_message("vals_interval",extra_pars3),
  row.names = NULL
)
expected4 <- NULL
expected5 <- data.frame(
  rows = c(1,4),
  test_name = test_name["vals_interval"],
  err_message = err_message("vals_interval",extra_pars5),
  row.names = NULL
)
expected6 <- data.frame(
  rows = c(5,6,7,9,10),
  test_name = test_name["vals_interval"],
  err_message = err_message("vals_interval",extra_pars6),
  row.names = NULL
)


#---------- VALUES IN A SET OF FACTORS ----------#
# Extra parameters for function
extra_pars1 <- list(vals = c("element1", 
                             "element2", 
                             "element3", 
                             "element4"),
                    cols = "factor")
extra_pars2 <- list(vals = c("element1", 
                             "element3"),
                    cols = "factor")

# Result
res1 <- vals_categ(dat$factor, extra_pars1)
res2 <- vals_categ(dat$factor, extra_pars2)

# Expected results
expected1 <- NULL
expected2 <- data.frame(
  rows = c(8,9),
  test_name = test_name["vals_categ"],
  err_message = err_message("vals_categ",extra_pars2),
  row.names = NULL
)


#---------- CONCATENATED COLUMNS ----------#
# Extra parameters for function
extra_pars1 <- list(concat_cols = c("concat_pre", "concat_post"),
                    sep = "-",
                    cols = "concat")

# Result
res1 <- concat(dat$concat, dat, extra_pars1)

# Expected results
expected1 <- NULL


#---------- UNIQUENESS COLUMNS ----------#
# Extra parameters for function
extra_pars1 <- list(cols = "integ")
extra_pars2 <- list(cols = "concat_post")

# Result
res1 <- uniqueness(dat$integ, extra_pars1)
res2 <- uniqueness(dat$concat_post, extra_pars2)

# Expected results
expected1 <- NULL
expected2 <- data.frame(
  rows = c(2,4,5,6,7,8,9),
  test_name = test_name["uniqueness"],
  err_message = err_message("uniqueness", extra_pars2),
  row.names = NULL
)


#---------- TESTS ----------#
# Missing Values
testthat("missing values", expect_equal(res1, expected1))
# Values inside interval
testthat("Values inside interval", expect_equal(res1, expected1))
testthat("Values inside interval", expect_equal(res2, expected2))
testthat("Values inside interval", expect_equal(res3, expected3))
testthat("Values inside interval", expect_equal(res4, expected4))
testthat("Values inside interval", expect_equal(res5, expected5))
testthat("Values inside interval", expect_equal(res6, expected6))
# Values inside vector of factors
testthat("Values inside set of factors", expect_equal(res1, expected1))
testthat("Values inside set of factors", expect_equal(res2, expected2))
# Concatenated columns
testthat("Concatenated columns", expect_equal(res1, expected1))
# Unique columns
testthat("Unique columns", expect_equal(res1, expected1))
testthat("Unique columns", expect_equal(res2, expected2))
