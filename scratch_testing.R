# Testing file

devtools::load_all("../seesaw")

devtools::test()

devtools::document()

?time_series_station
?usagrid
?interpolate_grid
?graph_interp

val <- trend_of_temps()
View(val)
mean(val$slope_coef, na.rm = TRUE)


# Generate grid points within the contiguous USA
grid <- grid_usa()

# Plot the grid points
ggplot() +
  geom_point(data = grid, aes(x = x, y = y), color = "blue") +
  theme_minimal() +
  labs(title = "Grid of Points within the Contiguous USA") +
  theme(plot.title = element_text(hjust = 0.5))


grid <- usagrid(200)
locs_pred <- grid[, c("x", "y")]
preds <- interpolate_grid("2024-03-01", "2024-03-31", "T_DAILY_AVG", 200)

# For now use ggplot to plot the interpolated values
graph_interp(preds,grid)

load("data/full_table.RData")
load("data/station_info.RData")
readRDS("data/usa_shp.rds")

?station_info
?full_table

# This did not quite work
shp_file <- readRDS("data/usa_shp.rds")

usethis::use_data(shp_file, internal = TRUE)

load("R/sysdata.rda")


# Plot a yearly cycle

cycle <- yearly_cycle_station("53878")
ggplot(data = cycle, aes(x = DOY, y = Expected_AVG_T)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Yearly Cycle of T_DAILY_AVG at Station 53878") +
  theme(plot.title = element_text(hjust = 0.5))

trend_of_temps()
