dir = "./NOAA_DATA"
folder_names = seq(2000,2024,length.out=25)
# First Dataset
# A dataset with with one row for each station, providing information about 
# the station identifier, station name, state, longitude, and latitude
# We will need to load in data from all of the years
# One row for each station, station identifier (WBANNO)
# Should be close to 236 rows
# Dictionary for converting state abbreviations to full names
state_lookup <- c("AL" = "Alabama",
                  "AK" = "Alaska",
                  "AZ" = "Arizona",
                  "AR" = "Arkansas",
                  "CA" = "California",
                  "CO" = "Colorado",
                  "CT" = "Connecticut",
                  "DE" = "Delaware",
                  "FL" = "Florida",
                  "GA" = "Georgia",
                  "HI" = "Hawaii",
                  "ID" = "Idaho",
                  "IL" = "Illinois",
                  "IN" = "Indiana",
                  "IA" = "Iowa",
                  "KS" = "Kansas",
                  "KY" = "Kentucky",
                  "LA" = "Louisiana",
                  "ME" = "Maine",
                  "MD" = "Maryland",
                  "MA" = "Massachusetts",
                  "MI" = "Michigan",
                  "MN" = "Minnesota",
                  "MS" = "Mississippi",
                  "MO" = "Missouri",
                  "MT" = "Montana",
                  "NE" = "Nebraska",
                  "NV" = "Nevada",
                  "NH" = "New Hampshire",
                  "NJ" = "New Jersey",
                  "NM" = "New Mexico",
                  "NY" = "New York",
                  "NC" = "North Carolina",
                  "ND" = "North Dakota",
                  "OH" = "Ohio",
                  "OK" = "Oklahoma",
                  "OR" = "Oregon",
                  "PA" = "Pennsylvania",
                  "RI" = "Rhode Island",
                  "SC" = "South Carolina",
                  "SD" = "South Dakota",
                  "TN" = "Tennessee",
                  "TX" = "Texas",
                  "UT" = "Utah",
                  "VT" = "Vermont",
                  "VA" = "Virginia",
                  "WA" = "Washington",
                  "WV" = "West Virginia",
                  "WI" = "Wisconsin",
                  "WY" = "Wyoming")
# Function that loads in a .txt file from NOAA and extracts the
# station identifier, station name, state, longitude, and latitude
# and returns it as a named vector
attribute_extract <- function(txt_file){
  # Read in the file
  txt = readLines(txt_file,n=1)
  # Look at the file name
  fn <- as.character(txt_file)
  # Extract just the station portion of the directory
  station_file <- strsplit(fn,split="/")[[1]][4]
  #Split the line into individual "words"
  words <- strsplit(txt,split=" ")[[1]]
  #Remove all "" elements of words
  words <- words[words != ""]
  # Extract the station identifier
  station_id = words[1]
  # Extract the station name
  station_name = strsplit(station_file,split="-")[[1]][3]
  #Remove the ".txt" from station_name
  station_name <- substr(station_name,1,nchar(station_name)-4)
  # Extract the state
  state_abbreviation <- substr(station_name,1,2)
  state = as.character(state_lookup[state_abbreviation])
  # Extract the longitude
  longitude = as.numeric(words[4])
  # Extract the latitude
  latitude = as.numeric(words[5])
  # Return the named vector
  return(c("station_id" = station_id, "station_name" = station_name, "state" = 
             state, "longitude" = longitude, "latitude" = latitude))
}

example_txt <- attribute_extract(
  "./NOAA_DATA/2000/CRND0103-2000-NC_Asheville_8_SSW.txt")

# Loop over all of the directories and extract the station information
# Save the directory information as a row in a data frame
station_info <- data.frame(matrix(ncol=5,nrow=0))
colnames(station_info) <- c("station_id","station_name",
                            "state","longitude","latitude")
print(station_info)
for (folder in folder_names){
  # Get the list of files in the directory
  files <- list.files(paste0(dir,"/",folder))
  # Loop over the files
  for (file in files){
    # Extract the station information
    station <- attribute_extract(paste0(dir,"/",folder,"/",file))
    # Add the station information to the data frame
    station_info <- rbind(station_info,station)
  }
}
# Remove duplicates
station_info <- unique(station_info)
# Rename columns
colnames(station_info) <- c("station_id","station_name","state",
                            "longitude","latitude")

# Second Dataset
# Around 11mb as an .RData file 
# Follow the video to see how to document the datasets 
# Package will have a data directory, the .Rdata files should be in this folder
# This script, and the true data will not be put into the package

# Question 4
# Write a function that can extract data from the .Rdata files and return
# data from just one station
# Dplyr group_by(station), summarize
# But this costs us another dependency, so maybe just use the loop!

# Question 5
# A function for estimating the yearly cycle for one station
# We want to estimate the expected temperature on each day
# This can be done using a regression of sines and cosines (use research)
# 2pid/360 or something similar
# Don't just take an average; fit a smooth curve instead using linear regression

# Question 6
# A function for estimating the trend of temperatures over time, in degrees C
# Use same idea as question 5

# Question 7
# Create a grid of points over the contiguous USA
# Should probably use an external package for this (maps)
# Extract grid centers that remain inside of the continguous USA
# Return as a lon, lat very long column dataframe with 2 columns
# Function should specify the resolution of the grid to allow for easier testing

# Question 8
# A function for interpolating data from the stations to a grid point within the
# contiguous USA. 
# Easiest way to interpolate is to use GpGp package on the lat lon of the points

# Question 9
# A function for plotting the gridded interpolations on a map
# R function (image) is a base R way of doing it
# The (fields) package image.plot() looks better
# Both take a matrix as an argument (square data)
# Thus, we cannot just use question 8, and we will probably need to add helpers

# Vignette Notes
# Use T_DAILY_AVG as the temp variable

# Question 1
# Make a map

# Question 2
# Fit a spatial model and plot an interpolated map of average temps for 
# March 2024. Consider including elevating in your model. 

# Question 3
# Estimate the warmest and coldest day of the year for each station, and plot
# those days on two maps. Think carefully about how to represent the days 
# numerically
# Write up a statistical analysis about how you did these trends
# E.g. GpGp

# Question 4
# Estimate yearly cycles for 10 different stations, highlight a diversity of 
# climates around the contiguous USA. Plot should indicate which cycle is from
# which station. 
# Relatively complicated data visualization. 
# Everything should be labeled, and visually should be appealing

# Question 5
# Do a linear trend overtime of the temperature. How is it changing?
# Should be done for each station. 
# Some stations have less data, which causes the standard errors to be higher
# for some than for others. 
# Restrict the plot to the points that have lower standard errors.
# Or try weighting estimates based on their standard errors. 

# Question 6
# Compare our results to a reputable source.




