
# Predictions of number of trips by hour. 

# Packages ----------------------------------------------------------------

library('data.table')
library('mlr')


# Import data -------------------------------------------------------------

trips <- readRDS('data/processed/trips.rds')
weather <- readRDS('data/processed/weather.rds')


# Dataset preparation -----------------------------------------------------

# aggreagte trips by hour of the day
trips <- trips[, start_time := strptime(start_time, format = '%Y-%m-%d %H')]
trips <- trips[, .(start_date = unique(start_date), 
                   hour_of_day = unique(hour_of_day),
                   weekday = unique(weekday),
                   month = unique(month),
                   year = unique(year),
                   count = uniqueN(trip_id)),
               by = start_time]

# add weather data
trips <- merge(x = trips, 
               y = weather,
               by.x = 'start_date',
               by.y = 'date',
               all.X = TRUE)

# coerce columns to factor 
factor_cols <- c('weekday', 'month', 'hour_of_day', 'year', 'events_fog', 
                'events_rain', 'events_snow', 'events_thunderstorm')
trips[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]

# order trips by date and add rownames 
setorderv(trips, 'start_time')
rownames(trips) <- trips$start_time
trips[, c('start_time', 'start_date') := NULL]


# Conditional tree --------------------------------------------------------

# The task: create a regression task as we want to predict the numer of trips
prediction_task <- makeRegrTask(id = 'trips', data = trips, target = 'count')

# The learner
# what are the avaiable learner for the prediction task?
listLearners(obj = prediction_task)

# ctree
learner <- makeLearner("regr.ctree")

# Training the learner
set.seed(123456)
train_set <- sample(nrow(trips), (3 * nrow(trips)) / 4)
ctree_model <- train(learner = learner, task = prediction_task, subset = train_set)

# Predictions
test_set <- setdiff(1:nrow(trips), train_set)
ctree_pred <- predict(object = ctree_model, task = prediction_task, subset = test_set)

# The performance
# what are the availbale performance measures?
listMeasures(obj = prediction_task)

performance(ctree_pred, measures = mse)
# 269.9281


# Random forest -----------------------------------------------------------

prediction_task <- makeRegrTask(id = 'trips', data = trips, target = 'count')
learner <- makeLearner('regr.cforest', 
                       par.vals = list(ntree = 100))
model <- train(learner = learner, task = prediction_task, subset = train_set)
test_set <- setdiff(1:nrow(trips), train_set)
pred <- predict(object = model, task = prediction_task, subset = test_set)
performance(pred, measures = mse)
# 188.7767