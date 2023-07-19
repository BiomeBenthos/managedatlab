#' Quality assessments
#' 
#' 
#'
#' @param dat Dataframe. Dataset on which the quality assessment is performed
#' @param assertions List. List of data quality assertions to test using quality_test.* functions. This list must include the test name, the column(s) on which to perform the test and extra parameters to use in the quality_test.* function. 
#' 
#' @return List of two dataframe. Output: rows that passed the assessment and Error: rows that failed at least one test.
#'
#' @export

quality_assessment <- function(dat, assertions) {

  # Make sure args are given
  nstop(dat, "dat")
  nstop(assertions, "assertions")

  # Runs assertions test
  assertions_tests <- lapply(1:length(assertions), function(x) {
    test_quality(dat = dat,
                 assertions = assertions[[x]])
  }) |>
    do.call(what = rbind, args = _) |>
      tidyr::spread(data = _, key = test_name, value = err_message)

  output <- dat[-assertions_tests$rows,]
  err_df <- dat[assertions_tests$rows,]

  return(list(output = output,
              err_df = err_df,
              err_log = assertions_tests))

}
