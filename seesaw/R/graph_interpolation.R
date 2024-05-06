
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
#' @param grid A dataframe with columns x and y that represents the US grid.
#'
#' @return A ggplot2 object that graphs the interpolated values over the US grid.
#'
#' @examples
#' grid <- usagrid(50)
#' preds <- runif(nrow(grid),0,20)
#' graph_interp(preds, usagrid(50))
#'
#' @export


graph_interp <- function(preds, grid){

  # Check that preds is numeric
  if (!is.numeric(preds)) {
    stop("preds must be a numeric vector")
  }

  # Check that grid is a data frame with columns x,y
  if (!is.data.frame(grid) | !all(c("x", "y") %in% colnames(grid))) {
    stop("grid must be a data frame with columns x and y")
  }

  # Check that the length of preds is equal to the number of rows in grid
  if (length(preds) != nrow(grid)) {
    stop("length of preds must be equal to the number of rows in grid")
  }

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

