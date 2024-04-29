
test_that( "usagrid looks okay", {

  # Test 1: Create a grid of default resolution, argument supplied
  grid1 <- usagrid(50)

  # Check to make sure that the length of grid is less than the resolution
  # squared
  expect_equal(
    TRUE,
    ( length(grid1) <= 50^2 )
  )


  # Test 2: Create a grid of default resolution, argument omitted
  grid2 <- usagrid()

  # Check to make sure that defaults are functioning as expected
  expect_equal(
    grid1,
    grid2
  )


  # Test 3: Create a grid with specified resolution = 200
  grid3 <- usagrid( resolution = 200 )

  # Check to make sure that the number of rows in interpolation is less than the
  # resolution squared
  expect_equal(
    TRUE,
    ( length(grid2) <= 200^2 )
  )




} )
