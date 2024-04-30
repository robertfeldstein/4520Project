#'
#' A function for estimating the yearly cycle for one station.
#'
#'  A function for estimating the yearly cycle for one station. A yearly cycle is
#' simply the expected temperature on each day of the year.
#'
#' @param id The WBANNO of the station to estimate the yearly cycle for, formatted
#' as a character. Expects a WBANNO present in the station_info data frame.
#' @param variable The variable to estimate the yearly cycle for.
#' Default is "T_DAILY_AVG". Expects a variable present in the full_table
#' data frame.
#'
#' @return A data frame with the expected variable value for each day of the
#' year.
#'
#' @examples
#' yearly_cycle_station("03013")
#' 
#' @export

yearly_cycle_station <- function(id, variable = "T_DAILY_AVG"){

  # Load in files
  full_table_path <- system.file("Data", "full_table.RData", package = "seesaw")
  station_info_path <- system.file("Data", "station_info.RData", package = "seesaw")
  # Load the data file
  load(full_table_path)
  load(station_info_path)

  # Extract the station data
  station_data <- full_table[full_table$WBANNO == id,]

  # Add a day of year column
  station_data$DOY <- as.numeric(format(station_data$LST_DATE, "%j"))

  # Add sines and cosines to the data frame
  station_data$SIN_DOY <- sin(2 * pi * station_data$DOY / 365)
  station_data$COS_DOY <- cos(2 * pi * station_data$DOY / 365)

  #Perform linear regression
  formula_str <- paste(variable, "~ SIN_DOY + COS_DOY")
  lm_fit <- lm(formula_str, data = station_data)

  #Create a data frame with the expected temperature for each day
  expected_temps <- data.frame(DOY = 1:365)
  variable_name <- paste0("Expected_", variable)
  expected_temps[, variable_name] <- lm_fit$coefficients[1] +
    lm_fit$coefficients[2] * sin(2 * pi * expected_temps$DOY / 365) +
    lm_fit$coefficients[3] * cos(2 * pi * expected_temps$DOY / 365)

  return(expected_temps)

}




