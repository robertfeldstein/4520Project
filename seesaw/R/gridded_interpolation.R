# A function for interpolating data from the stations to a grid points within
# the contiguous USA.

# internal function
source("./R/grid_usa.R")

# external package
library(GpGp)


df <- load("./data/daily_data.RData")

# Var is the variable to interpolate over the stations
interpolate_grid <- function(var = "T_DAILY_AVG", resolution = 200, date =
                               Sys.Date()) {
  # Create a grid of points within the contiguous USA
  grid <- grid_usa(resolution)
  y <- full_table[, var]
  locs <- full_table[, c("LONGITUDE", "LATITUDE")]
  X <- model.matrix( ~ full_table$LST_DATE, data = full_table)
  # Fit the Gaussian process model
  model <- fit_model(y,locs, X, covfun_name = "matern_sphere", start_parms =
                       c(42.2746, 2.6493, 0.1902, 2.0873))
  grid$LST_DATE <- as.Date(date)
  X_pred <- model.matrix( ~ grid$LST_DATE, data = grid)
  locs_pred <- grid[, c("x", "y")]
  preds <- predictions(model,locs_pred,X_pred)
  return(preds)
}

grid <- grid_usa(200)
locs_pred <- grid[, c("x", "y")]
preds <- interpolate_grid()

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

