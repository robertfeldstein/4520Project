
test_that( "trend_of_temps looks okay", {

  # Test 1: Temperature trends for all stations, all dates
  trend1 <- trend_of_temps()

  # Check to make sure that all stations are included
  expect_equal(
    nrow(station_info),
    nrow(trend1)
  )

  # Check to make sure that the names of the columns are correct
  months <- c("January", "February", "March", "April", "May", "June", "July",
              "August", "September", "October", "November", "December")
  middle.columns <- paste0(months, "_slope")
  columns <- c("station_id", middle.columns, "mean")
  expect_equal(
    columns,
    names(trend1)
  )

  # Test 2: Temperature trends for a specific station, all dates
  station1 <- "22016"
  trend2 <- trend_of_temps(station1)

  # Check to make sure that only station of interest is included
  expect_equal(

  )

  # Check to make sure that the names of the columns are correct
  columns <- c("station_id", middle.columns, "mean")
  expect_equal(
    columns,
    names(trend1)
  )

  # Test 3: Temperature trends for a specific station, specific dates
  station2 <- "94084"
  start1 <- "2002-09-28"
  end1 <- "2014-08-11"
  trend3 <- trend_of_temps(station2, start1, end1)

  # Check to make sure that only station of interest is included

  # Check to make sure that only desired dates are included
  #  IS IT WORTH IT TO MAKE SURE MEANS ARE NOT THE SAME IF DIFF DATES

  # Check to make sure that the names of the columns are correct
  columns <- c("station_id", middle.columns, "mean")
  expect_equal(
    columns,
    names(trend1)
  )


} )
