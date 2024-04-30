#' A function for estimating the trend of temperatures over time,
#' in units of degrees Celsius per year.
#'
#' @param station_id A character vector of station IDs to include in the analysis.
#' If NULL, all stations are included. Station IDs should be in the format "99999".
#' @param date_start A character vector of the start date for the analysis. It
#' should be in the format "YYYY-MM-DD".
#' @param date_end A character vector of the end date for the analysis. It should
#' be in the format "YYYY-MM-DD".
#' @return A data frame with the station ID and a column for each month of the year.
#' The column value represents the slope of the temperature trend in degrees Celsius
#' of the first day of the month over the time period specified. One final column
#' in the dataframe stores the mean of the monthly slopes.
#' @examples
#' trend_of_temps(c("53878","04130"), "2000-01-01", "2020-12-31")

trend_of_temps <- function(station_id = NULL, date_start = "2000-01-01",
                           date_end = "2024-12-31"){
  # Load in files
  full_table_path <- system.file("Data", "full_table.RData", package = "seesaw")
  station_info_path <- system.file("Data", "station_info.RData", package = "seesaw")
  # Load the data file
  load(full_table_path)
  load(station_info_path)

  # Filter the data to include only the dates of interest
  full_table <- full_table[full_table$LST_DATE >= date_start
                           & full_table$LST_DATE <= date_end, ]

  # Subset the data to include only the stations of interest
  if ( !is.null(station_id) ){
    station_info <- station_info[station_info$station_id == station_id, ]
  }

  # Find the unique station ids
  stations <- station_info$station_id


  # Add a day and month column to the the data frame
  full_table$DAY <- as.numeric(format(full_table$LST_DATE, "%d"))
  full_table$MONTH <- as.numeric(format(full_table$LST_DATE, "%m"))

  # Initialize data frame to hold slope coefficients for each unique station_id
  slope_coefs <- data.frame(station_id = stations)

  # Loop through months of year
  months <- c("January", "February", "March", "April", "May", "June", "July",
              "August", "September", "October", "November", "December")
  for (month in months){
    # Create name vector
    namevector <- paste0("trend_", month)
    # Initialize a column (name vector) to hold slope coefficients for month
    slope_coefs[,namevector] <- NA
  }

  # Loop through stations
  for (station in 1:length(stations)) {
    # Initialize empty vector to hold slope coefficients for station
    station_slopes <- numeric(12)
    # Loop through months
    for (month in 1:12){
      # Filter the data to include only the station, month of interest
      # Use data from first of month
      station_data <- full_table[full_table$DAY == 1
                                 & full_table$MONTH == month
                                 & full_table$WBANNO == stations[station], ]
      # Move on to next month if there are less than 2 entries
      if (nrow(station_data) < 2) {
        next
      }
      # Fit linear model
      lm_fit <- lm(T_DAILY_AVG ~ LST_DATE, data = station_data)
      # Extract slope
      slope <- lm_fit$coefficients[2]

      # Convert the slope to units of degrees Celsius per year
      slope_degrees_per_year <- slope * 365
      # Store slope in station_slopes
      station_slopes[month] <- slope_degrees_per_year
    }
    # Store station_slopes in row corresponding to station
    slope_coefs[station, 2:13] <- station_slopes
  }
  # Find average slope coefficient for each station
  slope_coefs$trend_Overall <- rowMeans(slope_coefs[,2:13], na.rm = TRUE)

  return(slope_coefs)

}

