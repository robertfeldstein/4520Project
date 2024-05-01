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
<<<<<<< HEAD
#' yearly_cycle_station("53878")
#' 
=======
#' yearly_cycle_station("03013")
#'
>>>>>>> 98af3b539d0aedc013c7b4d1ebe5da91379c00e5
#' @export

yearly_cycle_station <- function(id, variable = "T_DAILY_AVG"){

  # Load tables
  data("full_table", package = "seesaw")
  data("station_info", package = "seesaw")
  
  # Check that id is a character and in station_info
  if(!is.character(id)){
    stop("id must be a character")
  }
  if(!(id %in% station_info$station_id)){
    stop("id must be a WBANNO present in station_info")
  }
  
  # Check that variable is in full_table
  if(!(variable %in% names(full_table))){
    stop("variable must be a variable present in full_table")
  }

  # Extract the station data
  station_data <- full_table[full_table$WBANNO == id,]

  # Add a day of year column
  station_data$DOY <- as.numeric(format(station_data$LST_DATE, "%j"))

  # Add sines and cosines to the data frame
  station_data$SIN_DOY <- sin(2 * pi * station_data$DOY / 365.25)
  station_data$COS_DOY <- cos(2 * pi * station_data$DOY / 365.25)

  # Add higher order sines and cosines to allow for more complex yearly cycles

  station_data$SIN_DOY2 <- sin(4 * pi * station_data$DOY / 365.25)
  station_data$COS_DOY2 <- cos(4 * pi * station_data$DOY / 365.25)

  #Perform linear regression
  formula_str <- paste(variable, "~ SIN_DOY + COS_DOY + SIN_DOY2 + COS_DOY2")
  lm_fit <- lm(formula_str, data = station_data)

  #Create a data frame with the expected temperature for each day
  expected_temps <- data.frame(DOY = 1:365)
  variable_name <- paste0("Expected_", variable)
  expected_temps[, variable_name] <- lm_fit$coefficients[1] +
    lm_fit$coefficients[2] * sin(2 * pi * expected_temps$DOY / 365.25) +
    lm_fit$coefficients[3] * cos(2 * pi * expected_temps$DOY / 365.25) +
    lm_fit$coefficients[4] * sin(4 * pi * expected_temps$DOY / 365.25) +
    lm_fit$coefficients[5] * cos(4 * pi * expected_temps$DOY / 365.25)

  return(expected_temps)

}

