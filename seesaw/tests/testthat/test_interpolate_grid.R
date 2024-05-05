
test_that( "interpolate_grid looks okay", {

  # Test 1: Test a User Defined X, y, Xpred, and resolution
  station_ids <- station_info$station_id

  # Filter data to include only March 2024, find average March 2024
  # temperature for each station
  march_data <- full_table[full_table$LST_DATE >= "2024-03-01"
                           & full_table$LST_DATE <= "2024-03-31", ]
  average_temps <- aggregate(march_data$T_DAILY_AVG,
                             by = list( march_data$WBANNO ),
                             FUN = mean )



  # Create a new data frame with station ids, average temperatures,lat and lon
  df <- data.frame(station_id = unique(march_data$WBANNO),
                   average_temp = average_temps$x)
  # Merge df with station_info to get lat and lon
  df <- merge(df, station_info, by.x = "station_id", by.y = "station_id")

  # Drop NAs
  df <- df[complete.cases(df), ]
  df$x <- df$longitude
  df$y <- df$latitude
  suppressWarnings({
  usa_boundary <- sf::st_crop(sf::st_make_valid(shp_file), xmin = -125, xmax = -66.93457,
                              ymin = 22.396308, ymax = 49.384358)
  })
  X <- cbind(1,df[, c("x", "y")])
  X$x <- as.numeric(X$x)
  X$y <- as.numeric(X$y)
  y <- as.numeric(average_temps$x)
  # Remove na from y
  y <- y[!is.na(y)]
  grid <- usagrid(50)
  Xpred <- as.data.frame(model.matrix( ~ x + y, data = grid))
  preds1 <- interpolate_grid(X= X, y = y, Xpred = Xpred, resolution = 50)


  # Check to make sure that the length of interpolation vector is equal to
  # number of rows in grid with same resolution
  expect_equal(
    nrow( usagrid(50) ),
    length(preds1)
  )

  # Test 2: Interpolation of default variable (T_DAILY_AVG) in 2008,
  #         with default resolution (200)
  preds2 <- interpolate_grid(X= X, y = y, Xpred = NULL, resolution = 50)
  # Check to make sure that defaults are functioning as expected
  expect_equal(
    length(preds1),
    length(preds2)
  )


} )
