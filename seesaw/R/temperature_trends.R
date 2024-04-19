# A function for estimating the trend of temperatures over time,
# in units of degrees Celsius per year.

trend_of_temps <- function(){

  # Load in files
  load("data/daily_data.RData")
  load("data/station_info.RData")


  stations <- unique(full_table$WBANNO)
  slope_coefs <- numeric(length(stations))

  full_table$DAY <- as.numeric(format(full_table$LST_DATE, "%d"))
  full_table$MONTH <- as.numeric(format(full_table$LST_DATE, "%m"))

  slope_coefs <- data.frame(station_id = stations)
  months <- c("January", "February", "March", "April", "May", "June", "July",
              "August", "September", "October", "November", "December")
  for (month in months){
    namevector <- paste0(month, "_slope")
    slope_coefs[,namevector] <- NA
  }

  for (station in 1:length(stations)) {
    print(station)
    # Filter the data to include only the station of interest
    for (month in 1:12){
      station_data <- full_table[full_table$DAY == 1
                                 & full_table$MONTH == month
                                 & full_table$WBANNO == stations[station], ]

    }
    # Filter the data to include only the station of interest

    if (nrow(station_data) < 2) {
      next
    }

    # Look at the trend of temperatures over time
    lm_fit <- lm(T_DAILY_AVG ~ LST_DATE, data = station_data)

    # Extract the slope of the linear model
    slope <- lm_fit$coefficients[2]

    # Convert the slope to units of degrees Celsius per year
    slope_degrees_per_year <- slope * 365
    slope_coefs[station] <- slope_degrees_per_year
  }

  # Make a data.frame with station id and slope_coef

  slope_coefs <- data.frame(station_id = stations,
                            slope_coef = slope_coefs)

  return(slope_coefs)
}

val <- trend_of_temps()
mean(val$slope_coef, na.rm = TRUE)



