
test_that( "yearly_cycle_station looks okay", {

  # Test 1: Standard function call (arbitrary station, "T_DAILY_AVG" as variable)
  station1 <- "03047"
  cycle1 <- yearly_cycle_station(station1, "T_DAILY_AVG")

  # Check to make sure that the column names are correct
  expect_equal(
    c("DOY", "Expected_T_DAILY_AVG"),
    names(cycle1)
  )

  # Check to make sure that the number of rows are correct
  expect_equal(
    365,
    nrow(cycle1)
  )


  # Test 2: Function call using default variable
  cycle2 <- yearly_cycle_station(station1)

  # Check to make sure that the column names are correct
  expect_equal(
    c("DOY", "Expected_T_DAILY_AVG"),
    names(cycle2)
  )

  # Check to make sure that the number of rows are correct
  expect_equal(
    365,
    nrow(cycle2)
  )

  # Check to make sure default is functioning correct
  expect_equal(
    cycle1,
    cycle2
  )


  # Test 3: Function call using a different variable
  station2 <- "27516"
  variable <- "T_DAILY_MIN"
  cycle3 <- yearly_cycle_station(station2, variable)

  # Check to make sure that the column names are correct
  expect_equal(
    c("DOY", "Expected_T_DAILY_MIN"),
    names(cycle3)
  )

  # Check to make sure that the number of rows are correct
  expect_equal(
    365,
    nrow(cycle3)
  )


} )
