#' Save curated data in data/curated folder
#' 
#' Function to save curated in different format
#'
#' @param x (character) Name of the object to save
#' @param filename (character) File name to save object to
#' @param fileformat (character) The format in which the data is save
#' @param geom (character) If fileformat if "gpkg", vector of character to specify the colnames of longitude and latitude
#'
#' @return NULL
#'
#' @export
save_dat <- function(x=NULL, filename = NULL, fileformat = NULL, geom = NULL, crs = NULL) {

  # Check arguments
  nstop(x, "x")
  nstop(filename, "filename")
  nstop(fileformat, "fileformat")
  if(fileformat == "gpkg") {
    nstop(geom, "geom")
    nstop(crs, "crs")
    cols_stop(x, geom)
  }

  # Save data depending on the output format
  switch(fileformat,
    csv = {
      write.csv(raw_dat,
                sprintf("data/curated/%s", metadat_file$clean_files_csv),
                row.names = FALSE)
    },
    csv2 = {
      write.csv2(raw_dat,
                 sprintf("data/curated/%s", metadat_file$clean_files_csv),
                 row.names = FALSE)
    },
    gpkg = {
      if(!"gpkg" %in% class(x)) {
        x <- terra::vect(x, geom = geom, crs = crs)
      }
      terra::writeVector(x, 
                         filename = sprintf("data/curated/%s.gpkg", filename),
                         filetype = fileformat,
                         overwrite = TRUE)
    }
  )
}
