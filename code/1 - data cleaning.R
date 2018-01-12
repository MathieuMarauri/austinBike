
# Pre-processing of the data (https://www.kaggle.com/jboysen/austin-bike for
# trips and stations and https://www.kaggle.com/grubenm/austin-weather/data for
# weather). Import the data, quality audit, feature cleaning and creation of new
# variables

# Packages ----------------------------------------------------------------

library('data.table')
library('stringi')
library('auditdata')
library('lubridate')

Sys.setlocale(category = "LC_TIME", locale = "english")


# Stations ----------------------------------------------------------------

# Import
stations <- fread(file = 'data/raw/austin_bikeshare_stations.csv')
stations <- stations[, .(station_id, latitude, longitude, name, status)]

#' 72 rows, 5 variables, one row per station
#' station_id: unique identifier of the station
#' latitude: latitude of the station
#' logitude: longitude of the station
#' name: name of the station
#' status: station status (active, closed, moved, ACL-only)

# audit
qualityCheck(data = stations,
             file = 'output/audit/stations.xlsx',
             verbose = FALSE)

saveRDS(stations, 'data/clean/stations.rds')
rm(stations)


# Trips -------------------------------------------------------------------

# Import
trips <- fread(file = 'data/raw/austin_bikeshare_trips.csv')
trips <- trips[, .(trip_id, start_station_id, end_station_id, start_time, 
                   duration_minutes, month, year, bike_id = bikeid,
                   subscriber_type)]

#' 649 231 rows, 10 variables, one row per trip
#' trip_id: unique identifier of the trip
#' start_station_id: unique identifier of the start station
#' end_station_id: unique identifier of the end station
#' start_time: timestamp of the beginning of the trip
#' duration_minutes: duration of the trip in minutes
#' month: month of the trip
#' year: year of the trip
#' bike_id: unique identifier of the bike used for the trip
#' subscriber_type: membership type

# audit
qualityCheck(data = trips,
             file = 'output/audit/trips.xlsx')

# start_time coerced to date variables + extraction of weekday and hour of the
# day
trips[, 'start_time' := as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S")]
trips[, c('start_date', 'hour_of_day', 'weekday') := list(
  as.Date(start_time, format = '%Y-%m-%d'),
  hour(start_time),
  weekdays(start_time)
)]

# correct month and year 
trips[, month := month(start_time)]
trips[, year := year(start_time)]


# rearrange columns
setcolorder(trips, c('trip_id', 'start_station_id', 'end_station_id', 'start_time', 
                     'start_date', 'hour_of_day', 'weekday', 'month', 'year', 
                     'duration_minutes', 'bike_id', 'subscriber_type'))

# audit
qualityCheck(data = trips,
             file = 'output/audit/trips.xlsx')

saveRDS(trips, 'data/clean/trips.rds')
rm(trips)


# Weather -----------------------------------------------------------------

# Import
weather <- fread(file = 'data/raw/austin_weather.csv')
names(weather) <- c('date', 'temp_high_f', 'temp_avg_f', 'temp_low_f', 
                    'dewpoint_high_f', 'dewpoint_avg_f', 'dewpoint_low_f',
                    'humidity_high_percent', 'humidity_avg_percent', 'humidity_low_percent',
                    'sealevelpressure_high_inches', 'sealevelpressure_avg_inches',
                    'sealevelpressure_low_inches', 'visibility_high_miles', 
                    'visibility_avg_miles', 'visibility_low_miles', 'wind_high_mph', 
                    'wind_avg_mph', 'wind_gust_mph', 'precipitation_inches', 'events')


#' 1 319 rows, 21 variables, one row by date with info on temperature, Dew
#' point, Humidity, sea level pressure, visibilty, wind speed, wind gust,
#' precipitation and meteorological events

# audit
qualityCheck(data = weather, 
             file = 'output/audit/weather.xlsx')

# date as date columns
weather$date <- as.Date(weather$date, format = '%Y-%m-%d')

# remove - to coerce them to numeric (all columns but date, precipitations and
# events)
cols <- names(weather)[-c(1, 20, 21)]
weather[, (cols) := lapply(X = .SD, 
                           FUN = function(x) stri_replace_all_fixed(str = x, 
                                                                    pattern = '-', 
                                                                    replacement = NA)),
        .SDcols = cols]
weather[, (cols) := lapply(X = .SD, FUN = as.numeric),
        .SDcols = cols]

# change T (trace) to 0.005 in precipitation and coerce to numeric
weather[, 'precipitation_inches' := stri_replace_all_fixed(str = precipitation_inches,
                                                             pattern = 'T',
                                                             replacement = '0.005')]
weather[, 'precipitation_inches' := as.numeric(precipitation_inches)]

# split events into 5 different variables
weather[, 'events' := stri_replace_all_fixed(str = tolower(events), 
                                             pattern = ' ',
                                             replacement = '')]

weather <- splitstackshape::cSplit_e(data = weather, 
                                     type = 'character', 
                                     split.col = 'events', 
                                     sep = ',', 
                                     drop = TRUE, 
                                     fill = 0)

# audit
qualityCheck(data = weather,
             file = 'output/audit/weather.xlsx')

saveRDS(weather, 'data/clean/weather.rds')
rm(weather, cols)

