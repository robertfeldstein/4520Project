#'
#' A function for interpolating data from the stations to a grid points within
#' the contiguous USA.
#'
#' The function takes a start, end date, variable, and resolution as input to
#' interpolate that variable over the lower 48 states during that time period.
#'
#' @param X The design matrix. Expects a dataframe with one column of 1s for
#' the intercept, an 'x' column for the longitude, and a 'y' column for the
#' latitude. Additional columns can be included to account for other
#' covariates.
#'
#' @param y The response variable that would like to be interpolated. Must be
#' a numeric vector with the same number of rows as X.
#'
#' @param Xpred The design matrix for the prediction grid. If NULL, the function
#' will create a grid of points over the contiguous USA at the
#' specified resolution. Xpred expects a dataframe with one column of 1s for
#' the intercept, an 'x' column for the longitude, and a 'y' column for the
#' latitude. Additional columns can be included to account for other covariates.
#'
#' @param resolution The resolution of the grid to interpolate the variable over.
#'
#' @return A data frame containing the interpolated values for the specified
#' variable over the contiguous USA.
#'
#' @examples
#'
#' X <- data.frame(x = c(runif(100,min = -102, max = -81)), y = c(runif(100,min = 31, max = 37)))
#' X <- as.data.frame(cbind(1, X))
#' y <- c(runif(100, min = 0, max = 20))
#' interpolate_grid(X, y, Xpred = NULL, resolution = 20)
#'
#' @export

interpolate_grid <- function(X, y, Xpred = NULL, resolution = 50){

  # Check that X is a data frame with a column of 1s, a column for x, and a column for y
  if (!is.data.frame(X) | !all(c("x", "y") %in% colnames(X))){
    stop("X must be a data frame with columns x and y")
  }

  # Check that y is numeric and has the same number of rows as X
  if (!is.numeric(y) | length(y) != nrow(X)){
    stop("y must be a numeric vector with the same number of rows as X")
  }

  # Check that there is no missing data in X or y
  if (any(is.na(y)) | any(is.na(X))){
    stop("X and y must not contain missing data")
  }

  # Check that Xpred is NULL or a data frame with a column of 1s, a column for x, and a column for y
  if (!is.null(Xpred)){
    if (!is.data.frame(Xpred) | !all(c("x", "y") %in% colnames(Xpred))){
      stop("Xpred must be a data frame with columns x and y")
    }
  }

  # Check that resolution is an integer
  if (!resolution %% 1 == 0){
    stop("resolution must be an integer")
  }

  # Create locs variable
  locs <- X[, c("x", "y")]

  # Fit the Gaussian process model
  model <- GpGp::fit_model(y,locs, X, covfun_name = "matern_sphere",
                           silent = T)

  # Make predictions
  if (is.null(Xpred)){
    # Create grid of points
    grid <- usagrid(resolution)
    # Create prediction matrix
    Xpred <- stats::model.matrix( ~ x + y, data = grid)
  }

  # Extract x,y coordinates of grid points
  locs_pred <- Xpred[, c("x", "y")]
  # Use model to predict response at grid points
  preds <- GpGp::predictions(model,locs_pred,Xpred)

  return(preds)

}


