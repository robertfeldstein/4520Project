# A function for creating a grid of points that fall within the contiguous USA.
# You may consider using external packages to get map data and find points
# inside a polygon. Your function should have argument(s) for controlling the
# resolution of the grid.

#External libraries
library(sf)

# Function to create a grid of points within the contiguous USA
grid_usa <- function(resolution=200) {
  # Perhaps this should be an RDS file?
  shapefile <- readRDS("data/usa_shp.rds")
  multi_polygon <- st_geometry(shapefile)
  # Get x and y coordinates of the multi-polygon
  x_coords <- st_coordinates(multi_polygon)[, 1]
  y_coords <- st_coordinates(multi_polygon)[, 2]

  # Create a grid of points within the bounding box of the multi-polygon
  x <- seq(min(x_coords), max(x_coords), length.out = resolution)
  y <- seq(min(y_coords), max(y_coords), length.out = resolution)
  grid <- expand.grid(x = x, y = y)
  grid <- st_as_sf(grid, coords = c("x", "y"), crs = 4269)
  grid <- st_transform(grid, crs = st_crs(shapefile))
  grid$inside <- st_within(grid, shapefile)
  # Check if grid$inside is a list or an integer
  grid$inside <- lengths(grid$inside) > 0
  # Return the grid points that are in the polygon
  grid <- grid[grid$inside, ]
  grid_df <- as.data.frame(grid)
  grid_df$coordinates <- st_coordinates(grid_df$geometry)
  grid_df$x <- grid_df$coordinates[, 1]
  grid_df$y <- grid_df$coordinates[, 2]
  grid_df <- grid_df[, c("x", "y")]

  # Filter out the points that are not in the United States bounding box
  # 24.396308 to 49.384358 degrees latitude in the north, and -125.0 to -66.93457

  grid_df <- grid_df[grid_df$x > -125.0 & grid_df$x < -66.93457 & grid_df$y >
                       24.396308 & grid_df$y < 49.384358, ]
  return(grid_df)
}

# Generate grid points within the contiguous USA
grid <- grid_usa()

library(ggplot2)
# Plot the grid points
ggplot() +
  geom_point(data = grid, aes(x = x, y = y), color = "blue") +
  theme_minimal() +
  labs(title = "Grid of Points within the Contiguous USA") +
  theme(plot.title = element_text(hjust = 0.5))



