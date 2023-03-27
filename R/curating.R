#' Curating raw data
#' 
#' This function uses metadata .yaml file to curate raw data. It makes quality assessments based on the "quality" section of the metadata file. It also convert date in "yyyy-mm-dd" format, time in "HH:MM:SS" format and lat/lon in decimal degree format. It also removes whitespace and uppercase in column names.
#'
#' @param metadat_path The path to metadata file
#'
#' @return Saves curated data in specified folder

curating <- function(metadat_path) {

  # Check if metadat_path is given
  nstop("metadat_path", metadat_path)

  # Import metadata
  metadat_file <- read_dat(metadat_path, format = "yaml")$files
  metadat_values_to_na <- read_dat(metadat_path, format = "yaml")$values_to_na
  metadat_quality <- read_dat(metadat_path, format = "yaml")$quality
  metadat_convertion <- read_dat(metadat_path, format = "yaml")$convertion

  # Import raw data
  raw_file <- raw_file_path(metadat_file$filedir_raw,
                            metadat_file$filename_raw,
                            metadat_file$format_raw)
  raw_dat <- read_dat(raw_file, 
                      metadat_file$format_raw,
                      metadat_file$sheet_xlsx)


  #---------- QUALITY ASSESSMENTS ----------#

  raw_dat <- quality_assessments(raw_dat = raw_dat,
                                 metadat_quality = metadat_quality,
                                 filename = metadat_file$filename_raw)


  #---------- CONVERTION OF DATA (UNITS, DATE, TIME, LAT/LON, COLS) ----------#

  # If unit of a measurement if given in the column name, create a column specifically for the unit of measurements
  if(metadat_convertion$units$make_unit_cols) {
    for(i in metadat_convertion$units$units_cols) {
      raw_dat <- make_unit_cols(raw_dat, i)
    }
  }

  # Convert lat/lon columns in degree decimal
  if(!metadat_convertion$coords$coords_format %in% c("dec_deg", "")) {
    coords_cols <- unlist(metadat_convertion$coords$coords_cols)
    raw_dat[,coords_cols] <- sapply(coords_cols, function(x) {
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

  # Format date
  if(!metadat_convertion$date$date_format %in% c("%Y-%m-%d", "")) {
    date_cols <- unlist(metadat_convertion$date$date_cols)
    raw_dat[,date_cols] <- sapply(date_cols, function(x) {
      raw_dat[,x] <- lubridate::ymd(raw_dat[,x])
      return(raw_dat[,x])
    })
  }

  # Format time
  if(!metadat_convertion$time$time_format %in% c("%H-%M-%S", "")) {
    time_cols <- unlist(metadat_convertion$date$time_cols)
    raw_dat[,time_cols] <- sapply(time_cols, function(x) {
      raw_dat[,x] <- lubridate::hms(raw_dat[,x])
      return(raw_dat[,x])
    })
  }

  # Remove parenthesis in colnames, makes everyting in lower case and replace space with underscore
  colnames(raw_dat) <- remove_parenthesis(colnames(raw_dat)) |>
    gsub(pattern = " ",
         replacement = "_",
         x = _) |>
      tolower()

  # Save curated data
  save_dat(raw_dat, 
           metadat_file$filename_curated,
           metadat_file$format_curated,
           geom = c("lon", "lat"),
           crs = "EPSG:4326")

}
