#' Converting and formatting date, time, geom, units columns
#' 
#' This set of functions convert and format data in five ways. It format date to "YYYY-mm-dd", it format time to "HH:MM:SS", it converts lat/lon to decimal degree, is create units columns if the unit of a measurements is initially given in the names of the measurement columns, and it remove every whitespace and capital letters from the column name. It's basically a minimal convertion and formatting to clean the datasets.
#'
#' @param raw_dat Dataframe. Dataset to format
#' @param metadat_convertion List with information about date, time and lat/lon columns names and format, and the measurements columns that contain unit in their name 
#'
#' @return formatted dataset
#' @export

format_raw_dat <- function(raw_dat, metadat_convertion) {

  # Making unit columns
  raw_dat <- make_unit_cols(raw_dat, metadat_convertion)

  # Formatting date
  raw_dat <- format_date(raw_dat, metadat_convertion)

  # Formatting time
  raw_dat <- format_time(raw_dat, metadat_convertion)

  # Formatting coords
  raw_dat <- format_coords(raw_dat, metadat_convertion)

  # Formatting colnames
  raw_dat <- format_colnames(raw_dat, metadat_convertion)

  return(raw_dat)

}


make_unit_cols <- function(raw_dat, metadat_convertion) {  
  
  # If unit of a measurement if given in the column name, create a column specifically for the unit of measurements
  if(metadat_convertion$units$make_unit_cols) {
    for(i in metadat_convertion$units$units_cols) {
      
      # Check if columns are in the df
      cols_stop(raw_dat, i)

      # Extract unit
      unit <- extract_parenthesis(i)

      # Replace colname without the parenthesis and the unit
      new_colname <- remove_parenthesis(i)
      colnames(raw_dat)[colnames(raw_dat) == i] <- new_colname

      # Create new colnames for the unit
      unit_cols <- sprintf("%s_unit",new_colname)
      raw_dat[,unit_cols] <- unit
      rm(list=c("unit", "new_colname", "unit_cols"))
    }
  }
  return(raw_dat)
}


format_date <- function(raw_dat, metadat_convertion) {
  
  # If date format is not in YYYY-mm-dd
  if(!metadat_convertion$date$date_format %in% c("%Y-%m-%d", "")) {
    # Get date colname
    date_cols <- unlist(metadat_convertion$date$date_cols)
    raw_dat[,date_cols] <- sapply(date_cols, function(x) {
      raw_dat[,x] <- lubridate::ymd(raw_dat[,x])
      return(raw_dat[,x])
    })
  }
  return(raw_dat)
}


format_time <- function(raw_dat, metadat_convertion) {
  # If time format is not in HH:MM:SS
  if(!metadat_convertion$time$time_format %in% c("%H-%M-%S", "")) {
    # Get time colname
    time_cols <- unlist(metadat_convertion$date$time_cols)
    raw_dat[,time_cols] <- sapply(time_cols, function(x) {
      raw_dat[,x] <- lubridate::hms(raw_dat[,x])
      return(raw_dat[,x])
    })
  }
  return(raw_dat)
}


format_coords <- function(raw_dat, metadat_convertion) {

  # If coords format are not in decimal degree
  if(!metadat_convertion$coords$coords_format %in% c("dec_deg", "")) {
    
    # Names of the lat/lon columns
    coords_cols <- unlist(metadat_convertion$coords$coords_cols)
    raw_dat[,coords_cols] <- sapply(coords_cols, function(x) {
      # Format lat/lon data so that conv_unit function from measurements package can detect the format and convert it in decimal degree
      raw_dat[,x] <- gsub("° ", " ", raw_dat[,x])
      raw_dat[,x] <- gsub("°", " ", raw_dat[,x])
      raw_dat[,x] <- gsub("'", "", raw_dat[,x])
      raw_dat[,x] <- gsub('"', "", raw_dat[,x])
      raw_dat[,x] <- gsub(',', ".", raw_dat[,x])
      raw_dat[,x] <- measurements::conv_unit(raw_dat[,x],
                                             metadat_convertion$coords$coords_format,
                                             "dec_deg")
      return(raw_dat[,x])
    })
  }
  return(raw_dat)
}


format_colnames <- function(raw_dat) {
  colnames(raw_dat) <- remove_parenthesis(colnames(raw_dat)) |>
    gsub(pattern = " ",
         replacement = "_",
         x = _) |>
      tolower()
  return(raw_dat)
}
