
# Predictions of number of trips by hour removing rows with missing values and
# high and low columns.

# Packages ----------------------------------------------------------------

library('data.table')
library('stringi')
library('mlr')

# Dataset preparation -----------------------------------------------------

# load data
trips <- readRDS('data/trips_model.rds')

# keep only columns that are not high or low
keep_cols <- names(trips)[!stri_detect_regex(str = names(trips), pattern = '(_low_|_high_)')]
trips <- trips[, .SD, .SDcols = keep_cols]

# remove rows with NA
trips <- trips[complete.cases(trips)]


# Initiate modelisation ---------------------------------------------------

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


# Generalized linear model ------------------------------------------------

# create the learner
learner <- makeLearner("regr.h2o.glm", id = 'glm')

# train the learner
model <- train(learner = learner, task = task, subset = train_set)

# make prediction
predictions <- predict(object = model, task = task, subset = test_set)

# performacne of the model
performance(predictions, measures = mse)
# 426.088


# Glmnet -----------------------------------------------------------------

# create the learner
learner <- makeLearner("regr.glmnet", id = 'glmnet')

# train the learner
model <- train(learner = learner, task = task, subset = train_set)

# make prediction
predictions <- predict(object = model, task = task, subset = test_set)

# performacne of the model
performance(predictions, measures = mse)
# 426.3779


# Glmboost -----------------------------------------------------------

# create the learner
learner <- makeLearner("regr.glmboost", id = 'glmboost')

# train the learner
model <- train(learner = learner, task = task, subset = train_set)

# make prediction
predictions <- predict(object = model, task = task, subset = test_set)

# performacne of the model
performance(predictions, measures = mse)
# 581.7805


# Svm ---------------------------------------------------------------------

# create the learner
learner <- makeLearner("regr.ksvm", id = 'svm')

# train the learner
model <- train(learner = learner, task = task, subset = train_set)

# make prediction
predictions <- predict(object = model, task = task, subset = test_set)

# performacne of the model
performance(predictions, measures = mse)
# 308.8566

# tune the hyperparameters
learner$par.set

# specify the parameter space
parameter_space <- makeParamSet(
  makeDiscreteParam(id = "C", values = c(0.5, 1.0, 1.5, 2.0))
)

# the optimization algorithm
optim_algo <- makeTuneControlGrid()

# define the resampling strategy
resampling <- makeResampleDesc(method = 'CV', iters = 4L)

# tune the parameters
results <- tuneParams(learner = learner, 
                      task = task, 
                      resampling = resampling,
                      par.set = parameter_space, 
                      control = optim_algo, 
                      measures = mse)


# Gbm ---------------------------------------------------------------------

# create the learner
learner <- makeLearner("regr.h2o.gbm", id = 'gbm')

# train the learner
model <- train(learner = learner, task = task, subset = train_set)

# make prediction
predictions <- predict(object = model, task = task, subset = test_set)

# performacne of the model
performance(predictions, measures = mse)
# 222.6598


# Random forest -----------------------------------------------------------

# create the learner
learner <- makeLearner("regr.h2o.randomForest", 
                       id = 'rf',
                       par.vals = list(ntrees = 200))

# train the learner
model <- train(learner = learner, task = task, subset = train_set)

# make prediction
predictions <- predict(object = model, task = task, subset = test_set)

# performacne of the model
performance(predictions, measures = mse)
# 139.602

# tune the hyperparameters
learner$par.set

# specify the parameter space
parameter_space <- makeParamSet(
  makeDiscreteParam(id = "ntrees", values = c(100, 200, 300, 400)),
  makeDiscreteParam(id = 'max_depth', values = c(15, 18, 21)),
  makeDiscreteParam(id = 'nbins', values = c(20, 30, 40, 50, 70, 100)),
  makeDiscreteParam(id = 'mtries', values = c(2, 4, 6, 8))
)

# the optimization algorithm
optim_algo <- makeTuneControlRandom(maxit = 200L)

# define the resampling strategy
resampling <- makeResampleDesc(method = 'CV', iters = 3L)

# tune the parameters
results <- tuneParams(learner = learner, 
                      task = task, 
                      resampling = resampling,
                      par.set = parameter_space, 
                      control = optim_algo, 
                      measures = mse)


# Deep learning -----------------------------------------------------------

# create the learner
learner <- makeLearner("regr.h2o.deeplearning", id = 'deeplearning')

# train the learner
model <- train(learner = learner, task = task, subset = train_set)

# make prediction
predictions <- predict(object = model, task = task, subset = test_set)

# performacne of the model
performance(predictions, measures = mse)
# 156.456

# tune the hyperparameters
learner$par.set

# specify the parameter space
parameter_space <- makeParamSet(
  makeDiscreteParam(id = "hidden", values = list('100_100' = c(100, 100), 
                                                 '100_200' = c(100, 200), 
                                                 '100_300' = c(100, 300), 
                                                 '200_100' = c(200, 100), 
                                                 '200_200' = c(200, 200), 
                                                 '200_300' = c(200, 300),
                                                 '300_100' = c(300, 100), 
                                                 '300_200' = c(300, 200), 
                                                 '300_300' = c(300, 300)))
)

# the optimization algorithm
optim_algo <- makeTuneControlGrid()

# define the resampling strategy
resampling <- makeResampleDesc(method = 'CV', iters = 3L)

# tune the parameters
results <- tuneParams(learner = learner, 
                      task = task, 
                      resampling = resampling,
                      par.set = parameter_space, 
                      control = optim_algo, 
                      measures = mse)

