
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


graph_interp <- function(preds,grid){

  # Extract x,y coordinates of grid points
  locs_pred <- grid[, c("x", "y")]
  # Plot interpolation
  ggplot2::ggplot(data = as.data.frame(preds), ggplot2::aes(x = locs_pred[,"x"],
                                                   y = locs_pred[,"y"],
                                                   fill = preds)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::coord_quickmap() +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(title = "Interpolated T_DAILY_AVG values",
                  x = "Longitude",
                  y = "Latitude")
}



