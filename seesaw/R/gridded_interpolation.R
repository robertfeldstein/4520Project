#'
#' A function for interpolating data from the stations to a grid points within
#' the contiguous USA.
#'
#' The function takes a start, end date, variable, and resolution as input to
#' interpolate that variable over the lower 48 states during that time period.
#'
#' @param start_date The start date to interpolate the variable for. Expects a
#' date in the format of the LST_DATE column in the full_table data frame. This
#' format is "YYYY-MM-DD".
#'
#' @param end_date The end date to interpolate the variable for. Expects a date
#' in the format of the LST_DATE column in the full_table data frame. This
#' format is "YYYY-MM-DD".
#'
#' @param var The variable to interpolate. Default is "T_DAILY_AVG". Variable
#' must be a column in the full_table data frame. For a list of variables, see
#' the full_table data frame documentation.
#'
#' @param resolution The resolution of the grid to interpolate the variable over.
#'
#' @return A data frame containing the interpolated values for the specified
#' variable over the contiguous USA.
#'
#' @examples
#' interpolate_grid("2024-03-01", "2024-03-31", "T_DAILY_AVG", 200)
#' 
#' @export

interpolate_grid <- function(start_date, end_date, var = "T_DAILY_AVG",
                             resolution = 200){
  # Load in files
  full_table_path <- system.file("Data", "full_table.RData", package = "seesaw")
  # Load the data file
  load(full_table_path)

  # Create a grid of points within the contiguous USA
  grid <- usagrid(resolution)

  # Filter the data to include only the dates of interest
  full_table <- full_table[(full_table$LST_DATE >= start_date) &
                             (full_table$LST_DATE <= end_date), ]
  # Store response variable
  y <- full_table[ , var]
  # Subset data frame and store locations
  locs <- full_table[, c("LONGITUDE", "LATITUDE")]
  # Create model matrix
  X <- model.matrix( ~ full_table$LST_DATE, data = full_table)
  # Fit the Gaussian process model
  model <- GpGp::fit_model(y,locs, X, covfun_name = "matern_sphere",
                           start_parms = c(42.2746, 2.6493, 0.1902, 2.0873), 
                           silent = T)
  # Add date column to grid of points
  if (start_date == end_date){
    grid$date <- start_date
    # Create prediction matrix
    X_pred <- model.matrix( ~ 1+ date, data = grid)
  } else {
    # Create sequence of dates from start_date to end_date
    dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
    # For each date, there should be length(grid) number of rows in X_pred
    ndat <- rep(dates, each = nrow(grid))
    # Create prediction matrix
    X_pred <- cbind(1,ndat)
  }
  # Extract x,y coordinates of grid points
  locs_pred <- grid[, c("x", "y")]
  # Use model to predict response at grid points
  preds <- GpGp::predictions(model,locs_pred,X_pred)

  return(preds)

}



