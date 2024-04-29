
#' Create a grid of points over the contiguous United States
#'
#' This function creates a grid of points over the contiguous United States.
#' The grid points are generated within the bounding box of the contiguous
#' United States and does not include points over Alaska, Hawaii, or any of
#' the territories.
#'
#' @param resolution The number of points to generate in each dimension of the
#' grid.
#'
#' @return A data frame containing the grid points within the contiguous
#' United States.
#'
#' @examples
#' grid_usa(100)

usagrid <- function(resolution=50) {

  # Load in the shapefile from the RData file
  shp_file_path <- system.file("Data", "shp_file.RData", package = "seesaw")
  # Load the data file
  load(shp_file_path)
  #shapefile <- load("data/shp_file.RData")
  # Construct polygon
  multi_polygon <- sf::st_geometry(shp_file)

  # Get x and y coordinates of the multi-polygon
  x_coords <- sf::st_coordinates(multi_polygon)[, 1]
  y_coords <- sf::st_coordinates(multi_polygon)[, 2]
  
  # Subset the coordinates to only include the contiguous United States
  subset_indices <- x_coords > -125.0 & x_coords < -66.93457 & y_coords > 22.396308 & y_coords < 49.384358
  x_coords <- x_coords[subset_indices]
  y_coords <- y_coords[subset_indices]
  # x_coords <- x_coords[x_coords > -125.0 & x_coords < -66.93457 & y_coords >
  #                       24.396308 & y_coords < 49.384358]
  # y_coords <- y_coords[x_coords > -125.0 & x_coords < -66.93457 & y_coords >
  #                       24.396308 & y_coords < 49.384358]

  # Create a grid of points within the bounding box of the multi-polygon
  x <- seq(min(x_coords), max(x_coords), length.out = resolution)
  y <- seq(min(y_coords), max(y_coords), length.out = resolution)
  grid <- expand.grid(x = x, y = y)

  # Convert grid into an sf object
  grid <- sf::st_as_sf(grid, coords = c("x", "y"), crs = 4269)
  grid <- sf::st_transform(grid, crs = sf::st_crs(shp_file))

  # Use sf package to determine the points that intersect the shapefile
  grid$inside <- sf::st_within(grid, shp_file)
  grid$inside <- lengths(grid$inside) > 0

  # Keep the grid points that are in the polygon
  grid <- grid[grid$inside, ]

  # Convert sf object back into a dataframe and reformat
  grid_df <- as.data.frame(grid)
  grid_df$coordinates <- sf::st_coordinates(grid_df$geometry)
  grid_df$x <- grid_df$coordinates[, 1]
  grid_df$y <- grid_df$coordinates[, 2]
  grid_df <- grid_df[, c("x", "y")]

  # Filter out the points that are not in the United States bounding box
  # Here we are removing Alaska, Hawaii, and the territories
  # grid_df <- grid_df[grid_df$x > -125.0 & grid_df$x < -66.93457 & grid_df$y >
  #                      24.396308 & grid_df$y < 49.384358, ]
  return(grid_df)

}




