
test_that( "time_series_station looks okay", {

  # Test 1: Standard Function Call (arbitrary station, no date range provided)
  station1 <- "94995"
  tseries1 <- time_series_station(station1)

  # Check that column names are correct
  colnames <- c("WBANNO","state","station_name","LST_DATE","CRX_VN",
                "LONGITUDE","LATITUDE", "T_DAILY_MAX","T_DAILY_MIN","T_DAILY_MEAN",
                "T_DAILY_AVG", "P_DAILY_CALC","SOLARAD_DAILY")
  expect_equal(
    colnames,
    names(tseries1)
  )

  # Check that station is correct
  expect_equal(
    station1,
    tseries1$WBANNO[1]
  )

  # Check that all instances with desired station are included
  expect_equal(
    sum( full_table$WBANNO == station1 ),
    nrow(tseries1)
  )


  # Test 2: Start/End Dates Specified
  station2 <- "03047"
  start2 <-"2008-01-01"
  end2 <- "2008-03-01"
  tseries2 <- time_series_station(station2, start2, end2)

  # Check that column names are correct
  colnames <- c("WBANNO","state","station_name","LST_DATE","CRX_VN",
                "LONGITUDE","LATITUDE", "T_DAILY_MAX","T_DAILY_MIN","T_DAILY_MEAN",
                "T_DAILY_AVG", "P_DAILY_CALC","SOLARAD_DAILY")
  expect_equal(
    colnames,
    names(tseries2)
  )

  # Check that station is correct
  expect_equal(
    station2,
    tseries2$WBANNO[1]
  )

  # Check that only desired dates are included
  expect_equal(
    TRUE,
    ( start2 <= min(tseries2$LST_DATE) )
  )
  expect_equal(
    TRUE,
    ( end2 >= max(tseries2$LST_DATE) )
  )


} )
