#' A function for estimating the trend of temperatures over time,
#' in units of degrees Celsius per year.
#'
#' @param station_id The WBANNO of the station to estimate the temperature trend for, formatted
#' as a character. Expects a WBANNO present in the station_info data frame.
#' @param date_start A character vector of the start date for the analysis. It
#' should be in the format "YYYY-MM-DD".
#' @param date_end A character vector of the end date for the analysis. It should
#' be in the format "YYYY-MM-DD".
#' @return A named list with the trend (defined as the average increase in
#' temperatures on the first of the month over the dates specified) and standard
#' error.
#' @examples
#' trend_of_temps("53878", "2000-01-01", "2020-12-31")
#'
#' @export

trend_of_temps <- function(station_id, date_start = "2000-01-01",
                           date_end = "2024-12-31"){

  # Check that station_id is a valid station id
  if (!(station_id %in% station_info$station_id)){
    stop("station_id must be a valid WBANNO station id")
  }

  # Check that date_start and date_end are both dates
  # formatted as "YYYY-MM-DD"
  if (!inherits(as.Date(date_start, format = "%Y-%m-%d") , "Date")){
    stop("date_start must be a date in the format 'YYYY-MM-DD'")
  }

  if (!inherits(as.Date(date_end, format = "%Y-%m-%d"), "Date")){
    stop("date_end must be a date in the format 'YYYY-MM-DD'")
  }

  # Check that date_start is before date_end
  if (as.Date(date_start) > as.Date(date_end)){
    stop("date_start must be before date_end")
  }

  # Filter the data to include only the date, station of interest
  full_table <- full_table[full_table$LST_DATE >= date_start
                           & full_table$LST_DATE <= date_end
                           & full_table$WBANNO == station_id, ]

  # Add a day and month column to the the data frame
  full_table$DAY <- as.numeric(format(full_table$LST_DATE, "%d"))
  full_table$MONTH <- as.numeric(format(full_table$LST_DATE, "%m"))

  # Initialize data frame to hold slope coefficients, MSE, n for each month
  months <- c("January", "February", "March", "April", "May", "June", "July",
              "August", "September", "October", "November", "December")
  slope_coefs <- data.frame(Month = months, Trend = NA, SE = NA, n = NA)

  # Loop through months
  for (month in 1:12){
    # Filter the data to include only the station, month of interest
    # Use data from first of month
    station_data <- full_table[full_table$DAY == 1
                              & full_table$MONTH == month, ]
    # Move on to next month if there are less than 2 entries
    if (nrow(station_data) < 2) {
      next
    }
    # Fit linear model
    lm_fit <- stats::lm(T_DAILY_AVG ~ LST_DATE, data = station_data)
    # Extract slope, SE
    slope <- lm_fit$coefficients[2]
    summ <- summary(lm_fit)
    if (nrow(summ$coefficients) >= 2){
      se <- summ$coefficients[2,2]
    } else {
      se <- NA
    }


    # Convert the slope to units of degrees Celsius per year
    slope_degrees_per_year <- slope * 365
    # Store slope, SE, n in corresponding row in slope_coefs
    slope_coefs[month, 2] <- slope_degrees_per_year
    slope_coefs[month, 3] <- se
    slope_coefs[month, 4] <- nrow(station_data)
  }
  # Find average slope coefficient
  trend <- mean(slope_coefs$Trend, na.rm = TRUE)

  # Find the standard error
  vars <- (slope_coefs$SE)^2 * slope_coefs$n
  varPooled <- sum( vars * ( slope_coefs$n - 1) ) / ( sum(slope_coefs$n) - 12 )
  SE <- sqrt(varPooled / sum(slope_coefs$n) )


  return(c(trend = trend, SE = SE))
}

