#' Import raw data
#' 
#' Function to import raw data from different formats from the raw_data
#'
#' @param x (character) Name of the file to import
#' @param format (character) Format of the file to import
#' @param sheet (character) If format if 'xls' or 'xlsx', specify which sheet(s) to import
#'
#' @return data.frame or a list for metadata
#'
#' @export
read_dat <- function(x=NULL, format=NULL, sheet=NULL, ...) {

  # Check arguments
  nstop(x, "x")
  nstop(format, "format")
  # Check sheet arguments
  if(format %in% c("xlsx", "xls") & is.null(sheet)) {
    stop("Le nom du ou des feuillets Excel doivent être spécifiés si le format du fichier de données brutes est en 'xlsx' ou 'xls'", call. = FALSE)
  }

  # Import depending on file format
  switch(format,
    xlsx = {
      readxl::read_excel(x, sheet = sheet) |>
        as.data.frame()
    },
    xls = {
      readxl::read_excel(x, sheet = sheet) |>
        as.data.frame()
    },
    yaml = {
      yaml::read_yaml(x)
    },
    yml = {
      yaml::read_yaml(x)
    },
    csv = {
      read.csv(x, check.names = FALSE, row.names = NULL, ...)
    },
    csv2 = {
      read.csv2(x, check.names = FALSE, row.names = NULL, ...)
    },
    gpkg = {
      sf::st_read(x)
    },
    shp = {
      sf::st_read(x)
    },
    geojson = {
      sf::st_read(x)
    }
  )
}
