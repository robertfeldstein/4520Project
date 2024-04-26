# A function for estimating the yearly cycle for one station. A yearly cycle is
# simply the expected temperature on each day of the year. The function should
# return a data frame with row for each day, a column for day number (1-365),
# and a column for the expected average temperature on each day

yearly_cycle_station <- function(id, variable = "T_DAILY_AVG"){
  # Load in files
  load("data/full_table.RData")
  load("data/station_info.RData")

  # Extract the station data
  station_data <- full_table[full_table$WBANNO == id,]
  station_data$DOY <- as.numeric(format(station_data$LST_DATE, "%j"))

  # Add sines and cosines to the dataset, then perform linear regression
  station_data$SIN_DOY <- sin(2 * pi * station_data$DOY / 365)
  station_data$COS_DOY <- cos(2 * pi * station_data$DOY / 365)

  #Perform linear regression

  lm_fit <- lm(variable ~ SIN_DOY + COS_DOY, data = station_data)

  #Create a data frame with the expected temperature for each day
  expected_temps <- data.frame(DOY = 1:365)
  expected_temps$Expected_AVG_T <- lm_fit$coefficients[1] +
    lm_fit$coefficients[2] * sin(2 * pi * expected_temps$DOY / 365) +
    lm_fit$coefficients[3] * cos(2 * pi * expected_temps$DOY / 365)

  return(expected_temps)

}





