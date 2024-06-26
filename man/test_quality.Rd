% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quality_tests.R
\name{test_quality}
\alias{test_quality}
\title{Quality checks}
\usage{
test_quality(dat, assertions)
}
\arguments{
\item{dat}{data.frame. Dataset on which to perform the test.}

\item{assertions}{character. Column on which to perform the test.}

\item{x}{vector.}

\item{...}{}
}
\value{
NULL or vector of integer representing the lines that contains error
}
\description{
Set of functions to check quality of raw data before they get converted to a .csv file.
}
