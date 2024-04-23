# Testing file

devtools::load_all("../seesaw")

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
library(ggplot2)
ggplot(data = as.data.frame(preds), aes(x = locs_pred[,"x"],
                                        y = locs_pred[,"y"], fill = preds)) +
  geom_tile() +
  scale_fill_viridis_c() +
  coord_quickmap() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Interpolated T_DAILY_AVG values")



