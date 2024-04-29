
test_that( "graph_interp looks okay", {

  # Test 1: Graphing interpolation of T_DAILY_AVG in 2008, resolution = 200
  #         throws no error
  interp1 <- interpolate_grid(start_date = "2008-01-01",
                              end_date = "2008-12-31",
                              var = "T_DAILY_AVG",
                              resolution = 200)
  grid1 <- usagrid(200)
  expect_no_error(
    graph_interp(interp1, grid1)
  )

  # Test 2: Graphing interpolation of default variable (T_DAILY_AVG) in 2008,
  #         with default resolution (200) throws no error
  interp2 <- interpolate_grid(start_date = "2008-01-01",
                              end_date = "2008-12-31")
  grid1 <- usagrid(200)
  expect_no_error(
    graph_interp(interp2, grid1)
  )

  # Test 3: Interpolation of different variable, different resolution
  variable <- "SOLARAD_DAILY"
  res <- 220
  interp3 <- interpolate_grid(start_date = "2022-05-01",
                              end_date = "2022-08-31",
                              var = variable,
                              resolution = res)
  grid3 <- usagrid(res)
  expect_no_error(
    graph_interp(interp3, grid3)
  )


} )
