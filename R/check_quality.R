#' Quality checks
#' 
#' Set of functions to check quality of raw data before they get converted to a .csv file. 
#'
#' @param x Dataframe to check
#' @param checked A vector of the name(s) of column(s) whose quality is checked.
#' @param refs A vector of the name(s) of the reference column(s) to which the checked_cols will be compared 
#' @param type Type of quality check to be performed, either both_zero_na, na_refs, both_not_zero, not_na, smaller, bigger, equal, inside_bbox, inside_timespan
#' @param metadat_file Metadata file related to the raw data that is checked
#' 
#' @section both_zero_na:
#' 
#' @section na_refs:
#' 
#' @section both_not_zero:
#' 
#' @section not_na:
#' 
#' @section values_unique_comb:
#' 
#' @section smaller: 
#'
#' @section larger:
#' 
#' @section equal:
#' 
#' @section inside_bbox:
#' 
#' @section inside_timespan:
#' 
#' 
#' @return NULL or vector of integer representing the lines that contains error
#' 

check_quality <- function(type, ...) {
  UseMethod("check_quality", type)
}

#' @export
#' @rdname check_quality
check_quality.default <- function(x=NULL, checked=NULL, refs=NULL, values=NULL, type=NULL,metadat_file=NULL, ...) {

}


#' @export
#' @rdname check_quality
check_quality.both_zero_na <- function(x=NULL, checked=NULL, type=type,metadat_file=NULL, ...) {

  # Check arguments
  nstop(x, "x")
  nstop(checked, "checked")
  nstop(metadat_file, "metadat_file")

  # Check if columns are in the df
  cols_stop(x, checked)

  # Check if any values are == 0
  check <- sapply(1:nrow(x), function(y) {
    # Line to check
    tmp <- x[y,checked]
    # Check if both are equal 0 or if both are > 0
    if(all(is.na(tmp))) return(NULL) # If all NAs
    cond <- all(tmp[!is.na(tmp)] == 0) | all(tmp[!is.na(tmp)] > 0)
    if(!cond) return(y)
    return(NULL)
  }) |>
    unlist()
  
  # If there is any error
  if(length(check) > 0) {
    
    # Drive R output to log file
    logfile <- sprintf("log/%s.log", metadat_file$filename_raw)
    sink(logfile, append = file.exists(logfile))
    
    # Write message for every line
    for(i in check) {
      cat(err_message(type=type, checked=checked, i=i))
    }

    # Close connection
    sink()

  }

  return(unlist(check))
}


#' @export
#' @rdname check_quality
check_quality.na_refs <- function(x=NULL, checked=NULL, refs=NULL, type=type, metadat_file=NULL, ...) {

  # Check arguments
  nstop(x, "x")
  nstop(checked, "checked")
  nstop(refs, "refs")
  nstop(metadat_file, "metadat_file")

  # Check if columns are in the df
  cols_stop(x, checked)
  cols_stop(x, refs)

  # Check if any values are == 0
  check <- sapply(1:nrow(x), function(y) {
    
    # Line to check
    tmp <- x[y,c(checked, refs)]
    
    # Check if checked !is.na when all refs !is.na
    if(any(is.na(tmp[,refs])) & any(!is.na(tmp[,checked]))) return(y)
    return(NULL)
  
  }) |>
    unlist()
  
  # If there is any error
  if(length(check) > 0) {
    
    # Drive R output to log file
    logfile <- sprintf("log/%s.log", metadat_file$filename_raw)
    sink(logfile, append = file.exists(logfile))
    
    # Write message for every line
    for(i in check) {
      cat(err_message(type=type, checked=checked, i=i))
    }

    # Close connection
    sink()

  }

  return(unlist(check))
}


#' @export
#' @rdname check_quality
check_quality.both_not_zero <- function(x=NULL, checked=NULL, refs=NULL, type=type, metadat_file=NULL, ...) {

  # Check arguments
  nstop(x, "x")
  nstop(checked, "checked")
  nstop(refs, "refs")
  nstop(metadat_file, "metadat_file")

  # Check if columns are in the df
  cols_stop(x, checked)
  cols_stop(x, refs)

  # Check if any values are == 0
  check <- sapply(1:nrow(x), function(y) {
    
    # Line to check
    tmp <- x[y,c(checked, refs)]
    
    # Check if checked !is.na when all refs !is.na
    if(all(tmp[,refs] != 0) & any(tmp[,checked] == 0 | is.na(tmp[,checked]))) return(y)
    return(NULL)
  
  }) |>
    unlist()
  
  # If there is any error
  if(length(check) > 0) {
    
    # Drive R output to log file
    logfile <- sprintf("log/%s.log", metadat_file$filename_raw)
    sink(logfile, append = file.exists(logfile))
    
    # Write message for every line
    for(i in check) {
      cat(err_message(type=type, checked=checked, i=i))
    }

    # Close connection
    sink()

  }

  return(unlist(check))
}


#' @export
#' @rdname check_quality
check_quality.not_na <- function(x=NULL, checked=NULL, type=type, metadat_file=NULL, ...) {

  # Check arguments
  nstop(x, "x")
  nstop(checked, "checked")
  nstop(metadat_file, "metadat_file")

  # Check if columns are in the df
  cols_stop(x, checked)

  # Check if any values are == 0
  check <- sapply(1:nrow(x), function(y) {
    
    # Line to check
    tmp <- x[y,checked]
    
    # Check if every column is not NA
    if(any(is.na(tmp))) return(y)
    return(NULL)
  
  }) |>
    unlist()
  
  # If there is any error
  if(length(check) > 0) {
    
    # Drive R output to log file
    logfile <- sprintf("log/%s.log", metadat_file$filename_raw)
    sink(logfile, append = file.exists(logfile))
    
    # Write message for every line
    for(i in check) {
      # Get columns that are NAs
      tmp <- x[check, checked]
      na_cols <- colnames(tmp)[is.na(tmp)]
      cat(err_message(type=type, checked=na_cols, i=i))
    }

    # Close connection
    sink()

  }

  return(unlist(check))

}


#' @export
#' @rdname check_quality
check_quality.values_unique_comb <- function(x=NULL, checked=NULL, refs = NULL, values = NULL, type=type, metadat_file=NULL, ...) {

  # Check arguments
  nstop(x, "x")
  nstop(checked, "checked")
  nstop(refs, "refs")
  nstop(values, "values")
  nstop(metadat_file, "metadat_file")

  # Check if columns are in the df
  cols_stop(x, checked)
  cols_stop(x, refs)

  # Get unique combinaisons
  unique_x <- unique(x[,refs])

  # Check if the unique combinaisons of "refs" has every "values" of "checked"
  check <- sapply(1:nrow(unique_x), function(y) {
    # Values to check
    check_vals <- merge(x, unique_x[y,], by = refs)[,"diameter (µm)"]
    
    # Check if every column is not NA
    if(!all(values %in% check_vals)) return(y)
    return(NULL)
  
  }) |>
    unlist()
  
  # If there is any error
  if(length(check) > 0) {
    
    # Drive R output to log file
    logfile <- sprintf("log/%s.log", metadat_file$filename_raw)
    sink(logfile, append = file.exists(logfile))
    
    # Write message for every line
    for(i in check) {
      # Get missing values
      check_vals <- merge(x, unique_x[i,], by = refs)
      miss_vals <- values[!values %in% check_vals[,"diameter (µm)"]]
      cat(err_message(type = type, 
                      checked = checked, 
                      refs = refs, 
                      values = miss_vals, 
                      i = i,
                      df = check_vals))
    }

    # Close connection
    sink()

  }

  check <- lapply(check, function(k) {
    paste0("x[,'",refs,"'] %in% unique_x[k,'",refs,"']",
                  collapse = " & ") |>
      parse(text = _) |>
        eval(expr = _) |>
          which()
  })

  return(unlist(unique(check)))

}


type_of_check <- function(type) {
  switch(
    type,
    both_zero_na = {
      structure("both_zero_na", class = "both_zero_na")
    },
    na_refs = {
      structure("na_refs", class = "na_refs")
    },
    both_not_zero = {
      structure("both_not_zero", class = "both_not_zero")
    },
    not_na = {
      structure("not_na", class = "not_na")
    },
    values_unique_comb = {
      structure("values_unique_comb", class = "values_unique_comb")
    },
    smaller = {
      structure("smaller", class = "smaller")
    },
    larger = {
      structure("larger", class = "larger")
    },
    equal = {
      structure("equal", class = "equal")
    },
    inside_bbox = {
      structure("inside_bbox", class = "inside_bbox")
    },
    inside_timespan = {
      structure("inside_timespan", class = "inside_timespan")
    }
  )
}


err_message <- function(type=NULL, checked=NULL, refs=NULL, values = NULL, i=NULL, df = NULL) {
  switch(
    type,
    both_zero_na = {
      sprintf("Erreur sur les colonnes %s à la ligne %i: au moins une des colonnes a une valeur de 0 alors que les autres ont des valeurs autres que 0. Si la données n'est pas disponible, mettre NA au lieu de 0. \n\n", paste0(checked, collapse = ", "), i)
    },
    na_refs = {
      sprintf("Erreur sur les colonnes %s à la ligne %i: Colonnes %s = NA. Les colonnes %s devraient aussi être NA. \n\n", paste0(checked, collapse = ", "), i, paste0(refs, collapse = ", "), paste0(checked, collapse = ", "))
    },
    both_not_zero = {
      sprintf("Erreur sur les colonnes %s à la ligne %i: L'information sur les colonnes %s est disponible, les colonnes %s devraient avoir des valeurs. \n\n", paste0(checked, collapse = ", "), i, paste0(refs, collapse = ", "), paste0(checked, collapse = ", "))
    },
    not_na = {
      sprintf("Erreur sur les colonnes %s à la ligne %i: Ces colonnes ne devrait pas avoir une valeur de NA. \n\n", paste0(checked, collapse = ", "), i)
    },
    values_unique_comb = {
      sprintf("Erreur lors de check_quality.values_unique_comb sur la colonne %s: Il manque les valeurs %s pour la combinaison unique de %s. \n\n", 
              checked,
              paste0(values, collapse = ", "),
              paste0(refs," = ",unique(df[,refs]), collapse = ", "))
    },
    smaller = {
      sprintf()
    },
    larger = {
      sprintf()
    },
    equal = {
      sprintf()
    },
    inside_bbox = {
      sprintf()
    },
    inside_timespan = {
      sprintf()
    }
  )
}
