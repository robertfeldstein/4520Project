
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
#' Durham, NC
#' time_series_station(54794, "2000-01-01", "2010-12-31")

time_series_station <- function(id, start_date = 11275, end_date = 19820){

  # Load in files
  load("data/full_table.RData")
  load("data/station_info.RData")

  # Extract the station data
  station_data <- full_table[full_table$WBANNO == id,]

  # Filter the data based on the start and end date
  station_data <- station_data[station_data$LST_DATE >= start_date &
                                 station_data$LST_DATE <= end_date,]

  return(station_data)

}
