nstop <- function(x, arg=NULL) {
  if (is.null(x)) stop(sprintf("L'argument %s doit être spécifié!", arg), call. = FALSE)
}

cols_stop <- function(x, cols) {
  
  # Make test
  test <- !cols %in% colnames(x)

  if(any(test)) {
    err <- ifelse(length(cols[test]) > 1,
                  sprintf("Les colonnes %s n'existent pas!", cols[test]),
                  sprintf("La colonne %s n'existe pas!", cols[test]))
    stop(err, call. = FALSE)
  }
}

raw_file_path <- function(filename, format, dir = "data/raw_data/") {
  sprintf("%s%s.%s", dir, filename, format)
}

extract_parenthesis <- function(x) {
  gsub("[\\(\\)]", "", regmatches(x, gregexpr("\\(.*?\\)", x))[[1]])
}

remove_parenthesis <- function(x) {
  gsub("\\s*\\([^\\)]+\\)","",x) |>
    trimws()
}

coords_to_dec <- function(x, from) {
  measurements::conv_unit(x, from = from, to = "dec_deg")
}

make_unit_cols <- function(x=NULL, colname=NULL) {

  # Check arguments
  nstop(x, "x")
  nstop(colname, "colname")

  # Check if columns are in the df
  cols_stop(x, colname)

  # Extract unit
  unit <- extract_parenthesis(colname)

  # Replace colname without the parenthesis and the unit
  new_colname <- remove_parenthesis(colname)
  colnames(x)[colnames(x) == colname] <- new_colname

  # Create new colnames for the unit
  unit_cols <- sprintf("%s_unit",new_colname)
  x[,unit_cols] <- unit

  return(x)
}