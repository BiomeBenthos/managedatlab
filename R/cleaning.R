#' Curating raw data
#' 
#' This function uses metadata .yaml file to curate raw data. It makes quality assessments based on the "quality" section of the metadata file. It also convert date in "yyyy-mm-dd" format, time in "HH:MM:SS" format and lat/lon in decimal degree format. It also removes whitespace and uppercase in column names.
#'
#' @param metadat_path The path to metadata file
#'
#' @return Saves curated data in specified folder
#' @export

cleaning <- function(metadat, dir_input, dir_output, dir_log) {

  # Import raw data
  raw_file <- data_file_path(dir_input,
                            metadat$files$filename_input,
                            metadat$files$format_input)
  raw_dat <- read_dat(raw_file, 
                      metadat$files$format_input,
                      metadat$files$sheet_xlsx)


  #---------- QUALITY ASSESSMENTS ----------#

  raw_dat <- quality_assessments(raw_dat = raw_dat,
                                 metadat_quality = metadat$quality,
                                 filename = metadat$files$filename_input,
                                 dir_log = dir_log)


  #---------- CONVERTION OF DATA (UNITS, DATE, TIME, LAT/LON, COLS) ----------#
  
  raw_dat <- format_raw_dat(raw_dat, metadat$convertion)


  #---------- SAVE DATA IN THE FORMAT SPECIFIED IN metadat_file ----------#

  # lat and lon columns
  lat <- remove_parenthesis(metadat$convertion$coords$coords_cols$lat) |>
    gsub(pattern = " ",
         replacement = "_",
         x = _) |>
      tolower()
  lon <- remove_parenthesis(metadat$convertion$coords$coords_cols$lon) |>
    gsub(pattern = " ",
         replacement = "_",
         x = _) |>
      tolower()

  # If longitude is positive, transform it to negative
  if(all(raw_dat[,c("long_start", "long_end")] > 0)) {
    raw_dat[,c("long_start", "long_end")] * -1
  }

  # Simplify save_dat and deal with lat/lon columns that change
  save_dat(raw_dat, 
           metadat$files$filename_output,
           dir_output,
           metadat$files$format_output,
           lat,
           lon,
           geom_type = metadat$convertion$coords$coords_type,
           crs = metadat$convertion$coords$coords_crs$crs)

}
