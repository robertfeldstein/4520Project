#'
#' NOAA Weather Station data between 2000 and 2024.
#'
#' A dataframe containing the daily weather data for each weather station in the U.S.
#' between 2000 and 2024.
#'
#' @format A data frame with 11 columns and 1134352 rows
#'
#' \describe{
#' \item{WBANNO}{The unique identifier for each weather station}
#' \item{LST_DATE}{The date of the weather data}
#' \item{CRX_VN}{The version number of the station datalogger program that was in
#' effect at the time of the observation. Note: This field should be
#' treated as text (i.e. string).}
#' \item{LONGITUDE}{The longitude of the weather station}
#' \item{LATITUDE}{The latitude of the weather station}
#' \item{T_DAILY_MAX}{The daily average temperature in Celsius}
#' \item{T_DAILY_MIN}{The daily minimum temperature in Celsius}
#' \item{T_DAILY_MEAN}{(The daily average calculated temperature in Celsius.
#'  Computed with this formula: T_DAILY_MAX + T_DAILY_MIN) / 2}
#' \item{T_DAILY_AVG}{The daily average temperature in Celsius}
#' \item{P_DAILY_CALC}{The daily precipitation in mm}
#' \item{SOLARAD_DAILY}{The daily solar radiation in MJ/m^2}
#' }
"full_table"
