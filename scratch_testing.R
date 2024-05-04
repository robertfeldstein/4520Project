# Testing file


devtools::document()

devtools::load_all()

devtools::test()


devtools::check()

devtools::install(build_vignettes = T)

export(usa_shp)
?usa_shp
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
old_shp_file <- readRDS("data/usa_shp.rds")

usethis::use_data(shp_file, internal = TRUE)

load("R/sysdata.rda")


# Plot a yearly cycle

library(ggplot2)
cycle <- yearly_cycle_station("53878","T_DAILY_MIN")
ggplot(data = cycle, aes(x = DOY, y = Expected_AVG_T)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Yearly Cycle of T_DAILY_AVG at Station 53878") +
  theme(plot.title = element_text(hjust = 0.5))

# library(sf)
# # Load shp file
# shp_file <- st_read("./usa_shp/")
# # Save shp_file as an RData file
# save(shp_file, file = "./data/shp_file.RData")
# all.equal(shp_file, old_shp_file)

usethis::use_vignette("seesaw")

better_interpolate <- function(X, y, Xpred = NULL, resolution = 50){
  
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
    Xpred <- model.matrix( ~ x + y, data = grid)
  } 

  # Extract x,y coordinates of grid points
  locs_pred <- Xpred[, c("x", "y")]
  # Use model to predict response at grid points
  preds <- GpGp::predictions(model,locs_pred,Xpred)
  
  return(preds)
  
}

# test interpolate function

# interpolate the average temperature for the month of March 2024
station_ids <- station_info$station_id

# Filter data to include only March 2024, find average March 2024 
# temperature for each station
march_data <- full_table[full_table$LST_DATE >= "2024-03-01" 
                         & full_table$LST_DATE <= "2024-03-31", ]
average_temps <- aggregate(march_data$T_DAILY_AVG,
                           by = list( march_data$WBANNO ),
                           FUN = mean )



# Create a new data frame with station ids, average temperatures,lat and lon
df <- data.frame(station_id = unique(march_data$WBANNO), 
                 average_temp = average_temps$x)
# Merge df with station_info to get lat and lon
df <- merge(df, station_info, by.x = "station_id", by.y = "station_id")

# Drop NAs
df <- df[complete.cases(df), ]
df$x <- df$longitude
df$y <- df$latitude
usa_boundary <- sf::st_crop(sf::st_make_valid(shp_file), xmin = -125, xmax = -66.93457, 
                        ymin = 22.396308, ymax = 49.384358)

X <- cbind(1,df[, c("x", "y")])
X$x <- as.numeric(X$x)
X$y <- as.numeric(X$y)
y <- as.numeric(average_temps$x)
# Remove na from y
y <- y[!is.na(y)]
X$elevation <- elevatr::get_elev_point(X[, c("x", "y")], prj = sf::st_crs(usa_boundary))$elevation

grid <- usagrid(50)
Xpred <- as.data.frame(model.matrix( ~ x + y, data = grid))
elevations <- elevatr::get_elev_point(grid[,c("x","y")], prj = sf::st_crs(usa_boundary))$elevation
Xpred$elevation <- elevations
preds <- better_interpolate(X= X, y = y, Xpred = Xpred, resolution = 50)

# For now use ggplot to plot the interpolated values
graph_interp(preds,grid)

X_extra <- cbind(1,X[, c("x", "y")])
new_preds <- better_interpolate(X= X_extra, y = y, Xpred = NULL, resolution = 50)
graph_interp(new_preds,grid)

X <- data.frame(x = c(runif(100,min = -102, max = -81)), y = c(runif(100,min = 31, max = 37)))
X <- as.data.frame(cbind(1, X))
y <- c(runif(100, min = 0, max = 20))
interpolate_grid(X, y, Xpred = NULL, resolution = 20)

usethis::use_mit_license()
