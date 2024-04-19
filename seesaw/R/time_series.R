#A function for extracting the time series for a specific station by station id.
#It should have optional
#arguments for the starting date and ending date of the time series.



time_series_station <- function(id, start_date = 11275, end_date = 19820){
  # Load in files
  load("data/daily_data.RData")
  load("data/station_info.RData")

  # Extract the station data
  station_data <- full_table[full_table$WBANNO == id,]

  # Filter the data based on the start and end date
  station_data <- station_data[station_data$LST_DATE >= start_date &
                                 station_data$LST_DATE <= end_date,]

  return(station_data)
}
