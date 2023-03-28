#' Curating raw data
#' 
#' This function uses metadata .yaml file to curate raw data. It makes quality assessments based on the "quality" section of the metadata file. It also convert date in "yyyy-mm-dd" format, time in "HH:MM:SS" format and lat/lon in decimal degree format. It also removes whitespace and uppercase in column names.
#'
#' @param metadat_path The path to metadata file
#'
#' @return Saves curated data in specified folder
#' @export

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
  
  raw_dat <- format_raw_dat(raw_dat, metadat_convertion)


  #---------- SAVE DATA IN THE FORMAT SPECIFIED IN metadat_file ----------#

  save_dat(raw_dat, 
           metadat_file$filename_curated,
           metadat_file$format_curated,
           geom = c("lon", "lat"),
           crs = "EPSG:4326")

}
