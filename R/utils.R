
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

data_file_path <- function(dir, filename, format) {
  sprintf("%s/%s.%s", dir, filename, format)
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

test_name <- c(
  "missing_vals" = "Valeur manquante",
  "vals_interval" = "Valeur à l'intérieur d'un intervalle",
  "vals_categ" = "Valeur à l'intérieur d'un ensemble d'un valeur",
  "sf_predicates" = "Prédicats binaires géométriques",
  "concat" = "La valeur de la colonne est la concaténation de plusieurs colonnes",
  "uniqueness" = "La valeur est unique"
)

err_message <- function(test, ...) {
  # Unpacking ellipsis
  list2env(..., envir = environment())
  switch(
    test,
    missing_vals = {
      sprintf("Il manque une valeur à la colonne %s", cols)
    },
    vals_interval = {
      sprintf("La valeur de la colonne %s est à l'extérieur de l'intervalle %s:%s",
              cols,
              as.character(min_vals),
              as.character(max_vals))
    },
    vals_categ = {
      sprintf("La valeur de la colonne %s n'est pas dans les choix %s",
              cols,
              paste0(vals, collapse = ", "))
    },
    sf_predicates = {
      sprintf("Le test %s a échoué", 
              list(...)$predicat)
    },
    concat = {
      sprintf("La colonne %s ne correspond pas à %s",
              cols,
              paste0(concat_cols, collapse = sep))
    },
    uniqueness = {
      sprintf("La valeur de la colonne %s n'est pas unique",
              cols)
    }
  )
}
