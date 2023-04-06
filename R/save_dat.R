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
save_dat <- function(x=NULL, filename = NULL, filedir = NULL, fileformat = NULL, lat = NULL, lon = NULL, geom_type = NULL, crs = NULL,...) {

  # Check arguments
  nstop(x, "x")
  nstop(filename, "filename")
  nstop(fileformat, "fileformat")

  # Save data depending on the output format
  switch(fileformat,
    csv = {
      filepath <- sprintf("%s/%s.csv", filedir, filename)
      write.csv(x,
                filepath,
                row.names = FALSE)
    },
    csv2 = {
      filepath <- sprintf("%s/%s.csv", filedir, filename)
      write.csv2(x,
                 filepath,
                 row.names = FALSE)
    },
    gpkg = {
      if(!"SpatVector" %in% class(x)) {
        
        # Check arguments and columns
        nstop(lon, "lon")
        nstop(lat, "lat")
        nstop(crs, "crs")
        nstop(geom_type, "geom_type")
        cols_stop(x, lat)
        cols_stop(x, lon)

        # Convert to spatVector lines
        if(geom_type == "lines") {
          x <- df_to_lines(x, lat, lon, crs)
        } else {
          x <- terra::vect(x, geom = geom, crs = crs)
        }

      }

      filepath = sprintf("%s/%s.gpkg", filedir, filename)
      terra::writeVector(x, 
                         filename = filepath,
                         filetype = fileformat,
                         overwrite = TRUE)
    }
  )
}
