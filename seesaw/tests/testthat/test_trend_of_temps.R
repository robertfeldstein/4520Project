
test_that( "trend_of_temps looks okay", {

  # Test 1: Temperature trends for  all dates
  station1 <- "22016"
  trend1 <- trend_of_temps(station1)

  # Check to make sure that function returns a list with correct names
  expect_equal(
    c("trend", "SE"),
    names(trend1)
  )

  # Check to make sure that trend, SE are numeric
  expect_equal(
    TRUE,
    (class(trend1["trend"]) == "numeric" & class(trend1["SE"]) == "numeric")
  )


  # Test 2: Temperature trends for specific dates
  station2 <- "94084"
  start1 <- "2002-09-28"
  end1 <- "2014-08-11"
  trend2 <- trend_of_temps(station2, start1, end1)

  # Check to make sure that function returns a list with correct names
  expect_equal(
    c("trend", "SE"),
    names(trend2)
  )

  # Check to make sure that trend, SE are numeric
  expect_equal(
    TRUE,
    (class(trend2["trend"]) == "numeric" & class(trend2["SE"]) == "numeric")
  )


} )
