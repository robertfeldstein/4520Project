#'
#' A shp file in sf format containing the boundaries of the entire United States.
#'
#' The file is taken from the U.S. 2020 census and is in the form of a shapefile.
#' The shp file is stored in a dataframe of 1 row and 4 columns.
#' @format sf
#'
#' \describe{
#'  \item{AFFGEOID}{American FactFinder Geographic Identifier}
#'  \item{GEOID}{Geographic Identifier}
#'  \item{Name}{United States}
#'  \item{geometry}{A multipolygon object containing the boundaries of the United States}
#' }

"shp_file"
