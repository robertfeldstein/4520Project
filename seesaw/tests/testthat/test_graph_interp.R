
test_that( "graph_interp looks okay", {

  # Test 1: Test that the function runs without error
  # All it requires is that the input length is the same as the grid length
  grid1 <- usagrid(50)
  interp1 <- seq(1, 100, length.out = nrow(grid1))
  expect_no_error(
    graph_interp(interp1, grid1)
  )


} )
