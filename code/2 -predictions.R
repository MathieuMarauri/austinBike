
# Predictions of number of trips by hour. 

# Packages ----------------------------------------------------------------

library('data.table')
library('mlr')


# Import data -------------------------------------------------------------

trips <- readRDS('data/processed/trips.rds')
weather <- readRDS('data/processed/weather.rds')


# Feature engineering -----------------------------------------------------

# aggreagte trips by hour of the day
trips <- trips[, start_time := strptime(start_time, format = '%Y-%m-%d %H')]
trips <- trips[, .(start_time = unique(start_time), 
                   start_date = unique(start_date), 
                   hour_of_day = unique(hour_of_day),
                   weekday = unique(weekday),
                   month = unique(month),
                   year = unique(year)),
               by = start_time]


