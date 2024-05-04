
#' Extract the time series for a specific station by station id.
#'
#' Obtain the NOAA weather forecast data for a specific station between the
#' years 2000 and 2024.
#'
#' @param id The station id to extract the time series for.
#' @param start_date The start date to extract the time series for. Expects a
#' date in the format of the LST_DATE column in the full_table data frame. This
#' format is "YYYY-MM-DD".
#' @param end_date The end date to extract the time series for. Expects a date
#' in the format of the LST_DATE column in the full_table data frame. This
#' format is "YYYY-MM-DD".
#' @return A data frame containing the time series for the specified station.
#' @examples
#' time_series_station(54794, "2000-01-01", "2010-12-31")
#'
#' @export

time_series_station <- function(id, start_date = 11275, end_date = 19820){

  # Load tables
  # data("full_table", package = "seesaw")
  # data("station_info", package = "seesaw")

  # Check that id is in the station_info data frame
  if (!(id %in% station_info$station_id)){
    stop("id must be in the station_info data frame")
  }

  # Check that start_date and end_date are both dates
  # formatted as "YYYY-MM-DD"

  if (!inherits(as.Date(start_date, format = "%Y-%m-%d"), "Date")){
    stop("start_date must be a date in the format 'YYYY-MM-DD'")
  }

  if (!inherits(as.Date(end_date, format = "%Y-%m-%d"), "Date")){
    stop("end_date must be a date in the format 'YYYY-MM-DD'")
  }

  # Extract the station data
  station_data <- full_table[full_table$WBANNO == id,]

  # Filter the data based on the start and end date
  station_data <- station_data[station_data$LST_DATE >= start_date &
                                 station_data$LST_DATE <= end_date,]

  return(station_data)

}
