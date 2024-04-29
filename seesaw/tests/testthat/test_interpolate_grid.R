
test_that( "interpolate_grid looks okay", {

  # Test 1: Interpolation of T_DAILY_AVG in 2008, resolution = 200
  interp1 <- interpolate_grid(start_date = "2008-01-01",
                              end_date = "2008-12-31",
                              var = "T_DAILY_AVG",
                              resolution = 200)

  ### WHAT OUTPUT DO WE EXPECT HERE? LIKE DO WE KNOW HOW MANY POINTS THERE WILL
  ### BE?

  # Test 2: Interpolation of default variable (T_DAILY_AVG) in 2008,
  #         with default resolution (200)
  interp2 <- interpolate_grid(start_date = "2008-01-01",
                              end_date = "2008-12-31")

  # Check to make sure that defaults are functioning as expected
  expect_equal(
    interp1,
    interp2
  )

  # Test 3: Interpolation of different variable, different resolution
  variable <- "SOLARAD_DAILY"
  res <- 220
  interp3 <- interpolate_grid(start_date = "2022-05-01",
                              end_date = "2022-08-31",
                              var = variable,
                              resolution = res)


} )
