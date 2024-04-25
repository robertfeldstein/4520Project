
#'
#' A function that graphs an interpolation of values over the US.
#'
#' The function takes a vector of predictions and graphs them over the US grid
#' at a given resolution. The function uses the `grid_usa` function from seesaw
#' to plot the predictions over the US grid.
#'
#' @param preds A vector of predictions to be interpolated over the US grid. Preds
#' should be generated from the `interpolate_grid` function.
#'
#' @param resolution The resolution of the grid to be used. The resolution should
#' match the resolution used in the `interpolate_grid` function.
#'
#' @return A ggplot2 object that graphs the interpolated values over the US grid.
#'
#' @examples
#' preds <- interpolate_grid("2024-03-01", "2024-03-31", "T_DAILY_AVG", 200)
#' graph_interp(preds, 200)


graph_interp <- function(preds,resolution){
  grid <- grid_usa(resolution)
  locs_pred <- grid[, c("x", "y")]
  ggplot2::ggplot(data = as.data.frame(preds), aes(x = locs_pred[,"x"],
                                                   y = locs_pred[,"y"],
                                                   fill = preds)) +
    geom_tile() +
    scale_fill_viridis_c() +
    coord_quickmap() +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(title = "Interpolated T_DAILY_AVG values")
}



