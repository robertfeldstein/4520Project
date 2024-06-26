% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shp_file_data.R
\docType{data}
\name{shp_file}
\alias{shp_file}
\title{A shp file in sf format containing the boundaries of the entire United
States.}
\format{
RData
\describe{
 \item{AFFGEOID}{American FactFinder Geographic Identifier}
 \item{GEOID}{Geographic Identifier}
 \item{Name}{United States}
 \item{geometry}{A multipolygon object containing the boundaries of
 the United States}
}
}
\usage{
shp_file
}
\description{
The file is taken from the U.S. 2020 census and is in the form of a
shapefile.
The shp file is stored in a dataframe of 1 row and 4 columns.
}
\keyword{datasets}
