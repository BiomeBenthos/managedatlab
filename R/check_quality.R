#' Quality checks
#' 
#' Set of functions to check quality of raw data before they get converted to a .csv file. 
#'
#' @param x Dataframe to check
#' @param checked A vector of the name(s) of column(s) whose quality is checked.
#' @param refs A vector of the name(s) of the reference column(s) to which the checked_cols will be compared 
#' @param type Type of quality check to be performed, either zero, zero_na, smaller, bigger, equal, inside_bbox, inside_timespan
#' 
#' @section both_zero_na:
#' 
#' @section na_refs:
#' 
#' @section both_not_zero:
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
#' @return NULL or vector of integer representing the lines that contains error
#' 

check_quality <- function(type, ...) {
  UseMethod("check_quality", type)
}

#' @exporte
#' @rdname check_quality
check_quality.default <- function(x=NULL, checked=NULL, refs=NULL, type=NULL,...) {

}


#' @export
#' @rdname check_quality
check_quality.both_zero_na <- function(x=NULL, checked=NULL, type=type, ...) {

  # Check arguments
  nstop(x, "x")
  nstop(checked, "checked")

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
check_quality.na_refs <- function(x=NULL, checked=NULL, refs=NULL, type=type, ...) {

  # Check arguments
  nstop(x, "x")
  nstop(checked, "checked")
  nstop(refs, "refs")

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
check_quality.both_not_zero <- function(x=NULL, checked=NULL, refs=NULL, type=type, ...) {

  # Check arguments
  nstop(x, "x")
  nstop(checked, "checked")
  nstop(refs, "refs")

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
check_latlon_na <- function(x=NULL, checked=NULL, ...) {

  nstop(x, "x")
  nstop(checked, "checked")

  # Check if columns are in the df
  cols_stop(x, checked)

  # Check if there is any NAs values
  na_coords <- any(is.na(x[,checked]))

  # Error message if there is any
  if(na_coords) {
    
    # Which rows
    rows <- which(is.na(x[,checked]))

    # Send error message
    logfile <- sprintf("log/%s.log", metadat_file$filename_raw)
    cat(err_message(type = "latlon_na", checked = checked, i = rows),
        file = logfile,
        append = file.exists(logfile))
    
    return(rows)
  }

  return(NULL)
}



#' @export
#' @rdname children
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


#' @export
#' @rdname children
err_message <- function(type=NULL, checked=NULL, refs=NULL, i=NULL) {
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
    },
    latlon_na = {
      sprintf("La colonne %s a des valeurs NA aux lignes %s \n\n", checked, paste0(i, collapse = ", "))
    }
  )
}
