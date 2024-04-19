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

# The full daily dataset with the following columns:
#   • WBANNO, state, station name, LST_DATE, CRX_VN, LONGITUDE, LATITUDE,
# T_DAILY_MAX, T_DAILY_MIN, T_DAILY_MEAN, T_DAILY_AVG, P_DAILY_CALC,
# SOLARAD_DAILY
# Convert missing value codes into NAs. The date column should be in R’s date format. Be sure to
# document your dataset, explaining what each column means.

extract_necessary <- function(txt_file){
  # Headers
  headers <- c("WBANNO","LST_DATE","CRX_VN","LONGITUDE","LATITUDE","T_DAILY_MAX",
               "T_DAILY_MIN","T_DAILY_MEAN","T_DAILY_AVG",
               "P_DAILY_CALC","SOLARAD_DAILY", "SUR_TEMP_DAILY_TYPE",
               "SUR_TEMP_DAILY_MAX", "SUR_TEMP_DAILY_MIN", "SUR_TEMP_DAILY_AVG",
               "RH_DAILY_MAX", "RH_DAILY_MIN", "RH_DAILY_AVG",
               "SOIL_MOISTURE_5_DAILY", "SOIL_MOISTURE_10_DAILY",
               "SOIL_MOISTURE_20_DAILY","SOIL_MOISTURE_50_DAILY",
               "SOIL_MOISTURE_100_DAILY", "SOIL_TEMP_5_DAILY",
               "SOIL_TEMP_10_DAILY", "SOIL_TEMP_20_DAILY",
               "SOIL_TEMP_50_DAILY", "SOIL_TEMP_100_DAILY")
  table <- read.table(txt_file,header=F)
  colnames(table) <- headers
  # Convert the date column to R's date format
  table$LST_DATE <- as.Date(as.character(table$LST_DATE),format = "%Y%m%d")
  # Convert missing value codes into NAs
  table[table == -9999] <- NA
  table[table == -99] <- NA



  #Subset the table to only include the necessary columns
  table <- table[,c("WBANNO","LST_DATE","CRX_VN","LONGITUDE","LATITUDE","T_DAILY_MAX",
                    "T_DAILY_MIN","T_DAILY_MEAN","T_DAILY_AVG",
                    "P_DAILY_CALC","SOLARAD_DAILY")]
  return(table)
}

tab <- extract_necessary("./NOAA_DATA/2012/CRND0103-2012-NC_Asheville_8_SSW.txt")

# Loop through all of the directories and extract the necessary information
# Combine into one data frame
full_table <- data.frame(matrix(ncol=11, nrow=1.25E6))
colnames(full_table) <- c("WBANNO","LST_DATE","CRX_VN","LONGITUDE","LATITUDE","T_DAILY_MAX",
                          "T_DAILY_MIN","T_DAILY_MEAN","T_DAILY_AVG",
                          "P_DAILY_CALC","SOLARAD_DAILY")

# Initialize a counter for row index
row_index <- 1

for (folder in folder_names){
  # Get the list of files in the directory
  files <- list.files(paste0(dir,"/",folder))
  # Loop over the files
  for (file in files){
    # Extract the necessary information
    table <- extract_necessary(paste0(dir,"/",folder,"/",file))
    # Get the number of rows in the extracted table
    num_rows <- nrow(table)
    # Add the rows to the full table
    full_table[row_index:(row_index + num_rows - 1), colnames(table) ] <- table
    # Update the row index
    row_index <- row_index + num_rows
  }
}

#Remove rows with all NAs
full_table <- full_table[ rowSums(is.na(full_table)) < ncol(full_table), ]

## If desired, convert LST_DATE from the number of days since Jan 1, 1970 to a
## R Date object
full_table$LST_DATE <- as.Date(full_table$LST_DATE, origin = "1970-01-01")

# Save the data frame as a .RData file
save(full_table,file="./seesaw/data/daily_data.RData")




