# A function for estimating the trend of temperatures over time,
# in units of degrees Celsius per year.

trend_of_temps <- function(){
  # Load in files
  load("data/full_table.RData")
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
    station_slopes <- numeric(12)
    # Filter the data to include only the station of interest
    for (month in 1:12){
      station_data <- full_table[full_table$DAY == 1
                                 & full_table$MONTH == month
                                 & full_table$WBANNO == stations[station], ]
      if (nrow(station_data) < 2) {
        next
      }
      lm_fit <- lm(T_DAILY_AVG ~ LST_DATE, data = station_data)
      slope <- lm_fit$coefficients[2]

      # Convert the slope to units of degrees Celsius per year
      slope_degrees_per_year <- slope * 365
      station_slopes[month] <- slope_degrees_per_year
    }
    slope_coefs[station, 2:13] <- station_slopes
  }
  slope_coefs$mean <- rowMeans(slope_coefs[,2:13], na.rm = TRUE)
  return(slope_coefs)
}



