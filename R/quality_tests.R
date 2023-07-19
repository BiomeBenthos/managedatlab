#' Quality checks
#' 
#' Set of functions to check quality of raw data before they get converted to a .csv file. 
#'
#' @param dat data.frame. Dataset on which to perform the test.
#' @param assertions character. Column on which to perform the test.
#' @param x vector.
#' @param min_vals
#' @param max_vals
#' @param values
#' @param spat_ref_file
#' @param subspat
#' @param name_spat_test
#' @param name_spat_ref
#' @param crs_test
#' @param crs_ref
#' @param predicate
#' @param concat_cols
#' @param sep
#' @param ...
#'
#' @return NULL or vector of integer representing the lines that contains error


test_quality <- function(dat, assertions) {

  match.fun(assertions$fun)(x = dat[,assertions$pars$cols],
                            assertions$pars)

}

missing_vals <- function(x, ...) {

  # Unpacking ellipsis
  list2env(x = ..., envir = environment())
  nstop(na.strings, "na.strings")
  nstop(cols, "cols")

  test_assert <- x %in% na.strings |> which()

  if(length(test_assert) == 0) return(NULL)
  
  data.frame(
    rows = test_assert,
    test_name = test_name["missing_vals"],
    err_message = err_message("missing_vals", ...),
    row.names = NULL
  )
}


vals_interval <- function(x, ...) {

  # Unpacking ellipsis
  list2env(x = ..., envir = environment())
  nstop(min_vals, "min_vals")
  nstop(max_vals, "max_vals")
  nstop(cols, "cols")

  test_assert <- which(x < min_vals | x > max_vals)

  if(length(test_assert) == 0) return(NULL)
  
  data.frame(
    rows = test_assert,
    test_name = test_name["vals_interval"],
    err_message = err_message("vals_interval", ...),
    row.names = NULL
  )
}

vals_categ <- function(x, ...) {

  # Unpacking ellipsis
  list2env(x = ..., envir = environment())
  nstop(vals, "vals")
  nstop(cols, "cols")

  test_assert <- which(!x %in% vals)
  
  if(length(test_assert) == 0) return(NULL)

  data.frame(
    rows = test_assert,
    test_name = test_name["vals_categ"],
    err_message = err_message("vals_categ", ...),
    row.names = NULL
  )
}


concat <- function(x, dat, ...) {

  # Unpacking ellipsis
  list2env(x = ..., envir = environment())
  nstop(concat_cols, "concat_cols")
  nstop(sep, "sep")
  nstop(cols, "cols")

  test_assert <- which(do.call(paste, c(dat[concat_cols], sep = sep)) != x)

  if(length(test_assert) == 0) return(NULL)
  
  data.frame(
    rows = test_assert,
    test_name = test_name["concat"],
    err_message = err_message("concat", ...),
    row.names = NULL
  )
}


uniqueness <- function(x, ...) {

  # Unpacking ellipsis
  list2env(x = ..., envir = environment())
  nstop(cols, "cols")

  test_assert <- which(duplicated(x) | duplicated(x, fromLast=TRUE))

  if(length(test_assert) == 0) return(NULL)
  
  data.frame(
    rows = test_assert,
    test_name = test_name["uniqueness"],
    err_message = err_message("uniqueness", ...),
    row.names = NULL
  )
}


sf_predicates <- function(dat, cols, spat_ref_file, subspat, name_spat_test, name_spat_ref, predicate, crs_test, crs_ref, ...) {
  if(!"sf" %in% class(dat)) {
    spat_test <- sf::st_as_sf(dat, coords = cols, crs_test)
  }
  spat_ref <- read_dat(spat_ref_file)
  # Look at crs of both object
  if(is.na(st_crs(spat_test))) st_crs(spat_test) <- crs_test
  if(is.na(st_crs(spat_ref))) st_crs(spat_ref) <- crs_ref
  if(!st_crs(spat_ref) %in% st_crs(spat_test)) {
    spat_ref <- st_transform(spat_ref, crs_test)
  }
  # If supspat is TRUE, test if test spatial object is inside a subpoly of the reference spatial object.
  test_assert <- lapply(1:nrow(spat_test), function(x) {
    if(subspat) {
      return(
        match.fun(sprintf("st_%s", predicate))(
          x = x,
          y = spat_ref[stringr::str_detect(
                         as.data.frame(x)[,name_spat_test], 
                         as.data.frame(spat_ref)[,name_spat_ref]
                       ),]
        )[[1]]
      )
    }
    match.fun(sprintf("st_%s", predicate))(
      x = x,
      y = spat_ref
    )
  }) |>
    sapply(X=_, FUN=function(x) !is.null(x)) |>
      which()

  if(length(test_assert) == 0) return(NULL)

  # Return dataframe with rows that failed, test name and error message
  data.frame(
    rows = test_assert,
    test_name = test_name[test],
    err_message = err_message(test, ...)
  )
}
