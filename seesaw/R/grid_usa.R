# A function for creating a grid of points that fall within the contiguous USA.
# You may consider using external packages to get map data and find points
# inside a polygon. Your function should have argument(s) for controlling the
# resolution of the grid.

#External library
library(sp)
library(spData)
library(maps)

# Function to create a grid of points within the contiguous USA
grid_usa <- function(resolution=50) {
  # Get the map data for the contiguous USA
  usa <- map("usa", plot = FALSE)

  # Make a grid of points
  x <- seq(usa$range[1], usa$range[2], length.out = resolution)
  y <- seq(usa$range[3], usa$range[4], length.out = resolution)
  grid <- expand.grid(x = x, y = y)

  # Remove NA from the map data
  XNA <- is.na(usa$x)
  YNA <- is.na(usa$y)
  usa$x <- usa$x[!XNA & !YNA]
  usa$y <- usa$y[!XNA & !YNA]

  # Make a polygon from the map data
  usa_polygon <- Polygon(cbind(usa$x, usa$y))

  # Check if each point is inside the polygon
  grid$inside <- point.in.polygon(grid$x, grid$y, usa$x, usa$y)

  # Return the grid points that are in the polygon
  grid <- grid[grid$inside==1, ]
  return(grid)
}

# Generate grid points within the contiguous USA
grid <- grid_usa()

# Plot the grid points
ggplot() +
  geom_point(data = grid, aes(x = x, y = y), color = "blue") +
  theme_minimal() +
  labs(title = "Grid of Points within the Contiguous USA") +
  theme(plot.title = element_text(hjust = 0.5))

