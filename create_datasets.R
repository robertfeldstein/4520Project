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
# Save the data frame as a .RData file
save(station_info,file="./seesaw/data/station_info.RData")
# Second Dataset
# Around 11mb as an .RData file 
# Follow the video to see how to document the datasets 
# Package will have a data directory, the .Rdata files should be in this folder
# This script, and the true data will not be put into the package





