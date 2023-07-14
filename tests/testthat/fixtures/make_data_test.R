# Make small dummy datasets to use for different tests

# Small dataset for basic tests
small_dataset <- data.frame(
  integ = c(1:10),
  num = runif(n = 10, min = 30, max = 60),
  concat_pre = sample(c("pre1", "pre2", "pre3"),
                      size = 10,
                      replace = TRUE),
  concat_post = sample(c(1:10),
                       size = 10,
                       replace = TRUE),
  factor = sample(c("element1", "element2", "element3"), 
                  size = 10, 
                  replace = TRUE) |>
             as.factor(),
  missing = c(1:4, NA, 6:9, NA)
)

small_dataset[,"concat"] <- paste0(small_dataset$concat_pre,
                                   "-",
                                   small_dataset$concat_post)

saveRDS(small_dataset,
        testthat::test_path("fixtures", "small_dataset.rds"))
