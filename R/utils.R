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