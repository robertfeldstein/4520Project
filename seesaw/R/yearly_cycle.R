# A function for estimating the yearly cycle for one station. A yearly cycle is
# simply the expected temperature on each day of the year. The function should
# return a data frame with row for each day, a column for day number (1-365),
# and a column for the expected average temperature on each day

yearly_cycle_station <- function(id){
  # Load in files
  load("data/daily_data.RData")
  load("data/station_info.RData")

  # Extract the station data
  station_data <- full_table[full_table$WBANNO == id,]
  station_data$DOY <- as.numeric(format(station_data$LST_DATE, "%j"))

}

station_data <- full_table[full_table$WBANNO == "53878",]
station_data$DOY <- as.numeric(format(station_data$LST_DATE, "%j"))
# Note: How do we deal with leap years? Some years have a 366th day?
# Note: How do we deal with missing data? Some days have no data?

# First: Add sines and cosines to the dataset, then perform linear regression

