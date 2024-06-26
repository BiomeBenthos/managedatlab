% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_dat.R
\name{save_dat}
\alias{save_dat}
\title{Save curated data in data/curated folder}
\usage{
save_dat(
  x = NULL,
  filename = NULL,
  filedir = NULL,
  fileformat = NULL,
  lat = NULL,
  lon = NULL,
  geom_type = NULL,
  crs = NULL,
  ...
)
}
\arguments{
\item{x}{(character) Name of the object to save}

\item{filename}{(character) File name to save object to}

\item{fileformat}{(character) The format in which the data is save}

\item{geom}{(character) If fileformat if "gpkg", vector of character to specify the colnames of longitude and latitude}
}
\description{
Function to save curated in different format
}
