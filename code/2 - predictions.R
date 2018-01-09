
# Predictions of number of trips by hour with all the columns and rows. 

# Packages ----------------------------------------------------------------

library('data.table')
library('mlr')


# Dataset preparation -----------------------------------------------------

# # import data
# trips <- readRDS('data/processed/trips.rds')
# weather <- readRDS('data/processed/weather.rds')
# 
# # aggreagte trips by hour of the day
# trips <- trips[, start_time := strptime(start_time, format = '%Y-%m-%d %H')]
# trips <- trips[, .(start_date = unique(start_date),
#                    hour_of_day = unique(hour_of_day),
#                    weekday = unique(weekday),
#                    month = unique(month),
#                    year = unique(year),
#                    count = uniqueN(trip_id)),
#                by = start_time]
# 
# # add weather data
# trips <- merge(x = trips,
#                y = weather,
#                by.x = 'start_date',
#                by.y = 'date',
#                all.X = TRUE)
# 
# # coerce columns to factor
# factor_cols <- c('weekday', 'month', 'hour_of_day', 'year', 'events_fog',
#                 'events_rain', 'events_snow', 'events_thunderstorm')
# trips[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]
# 
# # order trips by date and add rownames
# setorderv(trips, 'start_time')
# trips[, c('start_time', 'start_date') := NULL]
# 
# saveRDS(trips, 'data/trips_model.rds')


# Initiate modelisation ---------------------------------------------------

# load data
trips <- readRDS('data/trips_model.rds')

# create the task: regression task with target column 'count'
task <- makeRegrTask(id = 'trips', data = trips, target = 'count')

# what are the avialable learner for the prediction task?
list_learners <- listLearners(obj = task)

# what are the availbale performance measures?
listMeasures(obj = task)

# training and test set
set.seed(123456)
train_set <- sample(nrow(trips), (3 * nrow(trips)) / 4)
test_set <- setdiff(1:nrow(trips), train_set)


# Conditional tree --------------------------------------------------------

# create the learner
learner <- makeLearner("regr.ctree", id = 'ctree')

# train the learner
model <- train(learner = learner, task = task, subset = train_set)

# make prediction
predictions <- predict(object = model, task = task, subset = test_set)

# performacne of the model
performance(predictions, measures = mse)
# 247.2984


# Decison tree ------------------------------------------------------------

# create the learner
learner <- makeLearner("regr.rpart", id = 'rpart')

# train the learner
model <- train(learner = learner, task = task, subset = train_set)

# make prediction
predictions <- predict(object = model, task = task, subset = test_set)

# performacne of the model
performance(predictions, measures = mse)
# 410.919


# Random forest -----------------------------------------------------------

# create the learner
learner <- makeLearner('regr.cforest', 
                       id = 'rf',
                       par.vals = list(ntree = 100))

# train the learner
model <- train(learner = learner, task = task, subset = train_set)

# make prediction
predictions <- predict(object = model, task = task, subset = test_set)

# performacne of the model
performance(predictions, measures = mse)
# 188.7767


# Gradient boosting -------------------------------------------------------

# create the learner
learner <- makeLearner('regr.blackboost', 
                       id = 'gb')

# train the learner
model <- train(learner = learner, task = task, subset = train_set)

# make prediction
predictions <- predict(object = model, task = task, subset = test_set)

# performacne of the model
performance(predictions, measures = mse)
# 325.0193
