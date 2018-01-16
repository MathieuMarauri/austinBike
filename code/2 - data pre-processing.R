
# Data is pre-processed before the actual modelisation is done. A dataset with
# all the available data is built (merging trips and weather). Some treatments
# are then done to impute missing values and numeric variables. Besides some
# feature selection can also be performed since there 3 variables by indicator
# (temp, wind, ...). The outputs are a task (used in the mlr workflow) by
# datasets obtained after processing.

# Inputs: trips and weather clean datasets 

# Outputs: train and test set, one task by treatment.

# Packages ----------------------------------------------------------------

library('data.table') # dataset manipulation
library('mlr') # construct tasks and impute missing data
library('psych') # correlation matrix


# Base dataset ------------------------------------------------------------

# the base dataset for the modelisation is built. It is the trips with the
# weather data. Some columns are factorized (time indicators and events). 

# import data
trips <- readRDS('data/clean/trips.rds')
weather <- readRDS('data/clean/weather.rds')

# aggreagte trips by hour of the day
trips <- trips[, start_time := strptime(start_time, format = '%Y-%m-%d %H')]
trips <- trips[, .(start_date = unique(start_date),
                   hour_of_day = unique(hour_of_day),
                   weekday = unique(weekday),
                   month = unique(month),
                   year = unique(year),
                   count = uniqueN(trip_id)),
               by = start_time]

# add weather data to trips
trips <- merge(x = trips,
               y = weather,
               by.x = 'start_date',
               by.y = 'date',
               all.X = TRUE)

# coerce columns to factor so the learners know how to properly treat them
factor_cols <- c('weekday', 'month', 'hour_of_day', 'year', 'events_fog',
                'events_rain', 'events_snow', 'events_thunderstorm')
trips[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]

# order trips by date
setorderv(trips, 'start_time')
trips[, c('start_time', 'start_date') := NULL]

# save result and clean session
saveRDS(trips, 'data/tasks/trips.rds')
rm(weather, trips, factor_cols)


# Missing values imputation -----------------------------------------------

# rpart is used to impute missing values and construct a new dataset. Since it
# is a supervised way to impute data, it is done on a train set. 

# import trips
trips <- readRDS('data/tasks/trips.rds')

# creation of train and test set with a 0.75 split
set.seed(123456)
train_set <- sample(nrow(trips), (3 * nrow(trips)) / 4)
test_set <- setdiff(1:nrow(trips), train_set)

# where are the missing values?
sapply(X = trips, FUN = function(x) sum(is.na(x)))

# impute missing values with rpart as it can deal with missing feature values
# (NAs in visibility_high_miles will not be a problem to impute wind_gust_mph
# for example). There are missing values only in numeric variables. 
imputation <- impute(obj = as.data.frame(trips[train_set]),
                     target = 'count', 
                     classes = list(numeric = imputeLearner("regr.rpart")))

# impute missing values on the test set with the same model used for the train
# set
trips_test <- reimpute(obj = trips[test_set], 
                       desc = imputation$desc)

# bind train and test imputed data 
trips_imp <- rbind(imputation$data, trips_test)
setDT(trips_imp)

# clean session
rm(imputation, trips_test)


# Numeric variables --------------------------------------------------------

# Numeric variables can be scaled. Such treatment can lead to better
# performances, this will be tested latter with different learners. Creation of
# one dataset for each combination of treatment (imputation_scale and scale).
# Tables are coerced to data frames then back to DT to avoid modification by
# reference to impact the tables trips and trips_imp
trips_scale <- as.data.frame(trips)
setDT(trips_scale)
trips_imp_scale <- as.data.frame(trips_imp)
setDT(trips_imp_scale)

# Numeric variables are scaled. The mean and the sd of the variables are
# computed on the training set and the values obtained are used to then scale
# the numeric values on both the train and test set

# trips_scale
numeric_cols <- names(trips)[sapply(trips, class) == 'numeric']
scale_values <- lapply(X = trips_scale[train_set, .SD, .SDcols = numeric_cols],
                       FUN = function(x) c(mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)))
for(name in numeric_cols) {
  set(x = trips_scale, 
      j = name, 
      value = scale(x = trips_scale[[name]], 
                    center = scale_values[[name]][1], 
                    scale = scale_values[[name]][2]))
}

# trips_imp_scale
scale_values <- lapply(X = trips_imp_scale[train_set, .SD, .SDcols = numeric_cols],
                       FUN = function(x) c(mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)))
for(name in numeric_cols) {
  set(x = trips_imp_scale, 
      j = name, 
      value = scale(x = trips_imp_scale[[name]], 
                    center = scale_values[[name]][1], 
                    scale = scale_values[[name]][2]))
}

# clean session
rm(name, numeric_cols, scale_values)


# Features selection ------------------------------------------------------

# The weather variables are split into min, average and max by day. The feature
# selection process is used to decide which information should be kept (the min,
# the average or the max). Every indicator is kept, thefeature selection is only
# used here to keep one information by indicator. The correlations are compared
# and the variable with highest one is kept.

# Creation of one dataset by treatment. 
trips_fs <- as.data.table(trips)
setDT(trips_fs)
trips_scale_fs <- as.data.table(trips_scale)
setDT(trips_scale_fs)
trips_imp_fs <- as.data.table(trips_imp)
setDT(trips_imp_fs)
trips_imp_scale_fs <- as.data.table(trips_imp_scale)
setDT(trips_imp_scale_fs)

#'
#' This function computes the correlation matrix and returns the variables wwith
#' the highest correlation coefficient with the target variable (the last one of
#' the data argument).
#' 
#' @param data a data frame with the target variable as the last column
#' 
#' @return the column with the highest correaltion with the target variable.
#' 
highestCorr <- function(data, print = TRUE) {
  correlation <- corr.test(x = data, adjust = 'none')
  result <- names(which.max(correlation$r['count', -ncol(data)]))
  if(print) print(correlation)
  return(result)
}

# The process of finding the correlation is only applied on the datasets with
# and without imputation as scaling does not modify correlation.

# trips_sf and trips_scale_fs
columns_selected <- lapply(X = list(c('temp_low_f', 'temp_avg_f', 'temp_high_f'),
                                      c('dewpoint_low_f', 'dewpoint_avg_f', 'dewpoint_high_f'),
                                      c('humidity_low_percent', 'humidity_avg_percent', 'humidity_high_percent'),
                                      c('sealevelpressure_low_inches', 'sealevelpressure_avg_inches', 'sealevelpressure_high_inches'),
                                      c('visibility_low_miles', 'visibility_avg_miles', 'visibility_high_miles'),
                                      c('wind_avg_mph', 'wind_high_mph')),
                           FUN = function(variables) highestCorr(data = trips_fs[, .SD, .SDcols = c(variables, 'count')]))

trips_fs <- trips_fs[, .SD, .SDcols = c('hour_of_day', 'weekday', 'month', 'year', 
                                        unlist(columns_selected), 'wind_gust_mph', 
                                        'precipitation_inches', 'events_fog', 'events_rain',
                                        'events_snow', 'events_thunderstorm', 'count')]
trips_scale_fs <- trips_scale_fs[, .SD, .SDcols = c('hour_of_day', 'weekday', 'month', 'year', 
                                                    unlist(columns_selected), 'wind_gust_mph', 
                                                    'precipitation_inches', 'events_fog', 'events_rain',
                                                    'events_snow', 'events_thunderstorm', 'count')]

# trips_imp_fs and trips_imp_scale_fs
columns_selected_imp <- lapply(X = list(c('temp_low_f', 'temp_avg_f', 'temp_high_f'),
                                    c('dewpoint_low_f', 'dewpoint_avg_f', 'dewpoint_high_f'),
                                    c('humidity_low_percent', 'humidity_avg_percent', 'humidity_high_percent'),
                                    c('sealevelpressure_low_inches', 'sealevelpressure_avg_inches', 'sealevelpressure_high_inches'),
                                    c('visibility_low_miles', 'visibility_avg_miles', 'visibility_high_miles'),
                                    c('wind_avg_mph', 'wind_high_mph')),
                           FUN = function(variables) highestCorr(data = trips_imp_fs[, .SD, .SDcols = c(variables, 'count')]))

trips_imp_fs <- trips_imp_fs[, .SD, .SDcols = c('hour_of_day', 'weekday', 'month', 'year', 
                                                unlist(columns_selected_imp), 'wind_gust_mph', 
                                                'precipitation_inches', 'events_fog', 'events_rain',
                                                'events_snow', 'events_thunderstorm', 'count')]
trips_imp_scale_fs <- trips_imp_scale_fs[, .SD, .SDcols = c('hour_of_day', 'weekday', 'month', 'year', 
                                                            unlist(columns_selected_imp), 'wind_gust_mph', 
                                                            'precipitation_inches', 'events_fog', 'events_rain',
                                                            'events_snow', 'events_thunderstorm', 'count')]

# clean session
rm(columns_selected, columns_selected_imp, highestCorr)


# Mlr tasks ---------------------------------------------------------------

# One task by dataset is created. Tasks are the base of the mlr workflow. The
# target column along with the type of the task. 

# 8 tasks are created (3 different treatments possible so 2^3)
task <- makeRegrTask(id = 'trips', 
                     data = as.data.frame(trips), 
                     target = 'count')
task_imp <- makeRegrTask(id = 'trips_imp', 
                         data = as.data.frame(trips_imp), 
                         target = 'count')
task_scale <- makeRegrTask(id = 'trips_scale', 
                           data = as.data.frame(trips_scale), 
                           target = 'count')
task_fs <- makeRegrTask(id = 'trips_fs', 
                        data = as.data.frame(trips_fs), 
                        target = 'count')
task_imp_scale <- makeRegrTask(id = 'trips_imp_scale', 
                               data = as.data.frame(trips_imp_scale), 
                               target = 'count')
task_imp_fs <- makeRegrTask(id = 'trips_imp_fs', 
                            data = as.data.frame(trips_imp_fs), 
                            target = 'count')
task_scale_fs <- makeRegrTask(id = 'trips_scale_fs', 
                              data = as.data.frame(trips_scale_fs), 
                              target = 'count')
task_imp_scale_fs <- makeRegrTask(id = 'trips_imp_scale_fs', 
                                  data = as.data.frame(trips_imp_scale_fs), 
                                  target = 'count')

# save results and clean session
saveRDS(train_set, 'data/tasks/train_set.rds')
saveRDS(test_set, 'data/tasks/test_set.rds')
saveRDS(task, 'data/tasks/task.rds')
saveRDS(task_imp, 'data/tasks/task_imp.rds')
saveRDS(task_scale, 'data/tasks/task_scale.rds')
saveRDS(task_fs, 'data/tasks/task_fs.rds')
saveRDS(task_imp_scale, 'data/tasks/task_imp_scale.rds')
saveRDS(task_imp_fs, 'data/tasks/task_imp_fs.rds')
saveRDS(task_scale_fs, 'data/tasks/task_scale_fs.rds')
saveRDS(task_imp_scale_fs, 'data/tasks/task_imp_scale_fs.rds')

rm(list = ls())

