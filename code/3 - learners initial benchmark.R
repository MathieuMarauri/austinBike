
# The different tasks created are compared with several learners. As the
# structure of the datasets are not constant (with or without missing values)
# the comparison will only be done on few learners. The comparison is done on
# several performance measures. First no tuning is performed on the
# hyperparameters of the learners. This step will be done later on the top
# learners/tasks.

# Inputs: the tasks created in 2 - data pre-processing

# Outputs: the name of the couples task/learners that have the best performance.

# Packages ----------------------------------------------------------------

library('mlr') # benchmark learners
library('stringi') # string manipulation
library('ggplot2') # plot benchmark results


# Tasks and available learners --------------------------------------------

# The available learners and performance measures are listed for each task.

# import the tasks
task_names <- list.files(path = 'data/tasks', pattern = 'task')
task_names <- stri_replace_first_fixed(str = task_names, 
                                       pattern = '.rds', 
                                       replacement = '')
for(name in task_names) {
  assign(x = name, 
         value = readRDS(paste0('data/tasks/', name, '.rds')))
}

# What are the available learners for the datasets with missing values? A list
# of all the available learners for a regression task can be found here:
# https://mlr-org.github.io/mlr-tutorial/release/html/integrated_learners/index.html#regression-64
learners_missing <- listLearners(obj = task)

# What are the available learners for the datasets without missing values?
learners_imp <- listLearners(obj = task_imp)

# What are the available performance measures? See
# https://mlr-org.github.io/mlr-tutorial/release/html/measures/index.html#regression
# for some precisions on the measures
listMeasures(obj = task)

# clean session
rm(name, task_names)


# Benchmark on task with missing data -------------------------------------

# The task with a datasets containing missing data are compared first. The
# algorithms that are used are rpart, ctree, cforest and blackboost. The
# performance measure used to compare the models are mse, medse and the times of
# training and prediction.

# Construct the list of learners to test
learner_ctree <- makeLearner("regr.ctree", id = 'ctree')
learner_rpart <- makeLearner("regr.rpart", id = 'rpart')
learner_cforest <- makeLearner('regr.cforest', id = 'cforest')
learner_blackboost <- makeLearner('regr.blackboost', id = 'blackboost')

# Resampling strategy. Holdout is used with pre-defined train and test set. The
# performance measures will be computed on the same test set.
train_set <- readRDS('data/tasks/train_set.rds')
test_set <- readRDS('data/tasks/test_set.rds')
resampling <- makeFixedHoldoutInstance(train.inds = train_set, 
                                       test.inds = test_set, 
                                       size = task$task.desc$size)

# List of performance measures
perf_measures <- list(mse, medse, timetrain, timepredict)

# Conduct the benchmark. ALl learners are applied on all tasks. Only the
# performance measures are returned.
benchmark_missing <- benchmark(learners = list(learner_ctree, learner_rpart, 
                                               learner_cforest, learner_blackboost),
                               tasks = list(task, task_fs, task_scale, task_scale_fs),
                               resamplings = resampling,
                               measures = perf_measures, 
                               keep.pred = FALSE, 
                               models = FALSE)

# Keep only the results as a data frame
benchmark_missing <- getBMRAggrPerformances(bmr = benchmark_missing, as.df = TRUE)
saveRDS(benchmark_missing, 'data/models/benchmark_missing.rds')


# Benchmark on task with imputed data -------------------------------------

# The task with datasets with imputed data are compared here. The algorithms
# used are the same for the tasks with missing data plus gbm, glm, deeplearning,
# classic randomForest, deepLearning, glmnet and svm. The performance measures
# are the same as the ones used in previous section. Likely the sampling
# strategy is the one used before.

# Construct the list of learners to test
learner_gbm <- makeLearner('regr.h2o.gbm', id = 'gbm')
learner_glm <- makeLearner('regr.h2o.glm', id = 'glm')
learner_glmnet <- makeLearner('regr.glmnet', id = 'glmnet')
learner_deeplearning <- makeLearner('regr.h2o.deeplearning', id = 'deeplearning')
learner_randomforest <- makeLearner('regr.h2o.randomForest', id = 'randomForest')
learner_svm <- makeLearner('regr.ksvm', id = 'svm')

# benchmark learners
benchmark_imputed <- benchmark(learners = list(learner_ctree, learner_rpart, learner_svm,
                                               learner_cforest, learner_blackboost,
                                               learner_gbm, learner_glm, learner_glmnet, 
                                               learner_deeplearning, learner_randomforest),
                               tasks = list(task_imp, task_imp_fs, task_imp_scale, task_imp_scale_fs),
                               resamplings = resampling,
                               measures = perf_measures, 
                               keep.pred = FALSE, 
                               models = FALSE)

# Keep only the results as a data frame
benchmark_imputed <- getBMRAggrPerformances(bmr = benchmark_imputed, as.df = TRUE)
saveRDS(benchmark_imputed, 'data/models/benchmark_imputed.rds')

# clean session
rm(list = c(ls(pattern = 'learner'), ls(pattern = 'task'), 'resampling', 'train_set', 'test_set', 'perf_measures'))


# Select best tasks and learners ------------------------------------------

# The results from the previous benchmarks are merged and the best couples
# task/learner are selected to be tuned. The mse and medse are used to rank the
# couples task/learner. Times to train and predict are used to differentiate
# couples that are closed on the performance measures.

# merged results
benchmark_results <- rbind(benchmark_missing, benchmark_imputed)

# add indicator of imputation to better visualize differences
benchmark_results$imputed <- stri_detect_fixed(str = benchmark_results$task.id, pattern = 'imp')

# plot results to see which learner performed the best and which task
ggplot(data = benchmark_results, mapping = aes(x = learner.id, y = mse.test.mean)) + 
  geom_boxplot(fill = 'dodgerblue', alpha = .5, color = 'dodgerblue')

ggplot(data = benchmark_results, mapping = aes(x = task.id, y = mse.test.mean)) + 
  geom_boxplot(fill = 'dodgerblue', alpha = .5, color = 'dodgerblue')

# all combined
ggplot(data = benchmark_results, mapping = aes(x = mse.test.mean, y = learner.id, color = task.id, shape = imputed)) + 
  geom_point() + 
  scale_color_brewer(palette = 'Dark2')

# The learners applied to all tasks performed better on tasks without imputation
# but other learner have better performances, namely randomForest and
# deeplearning. To select the best task the performances of only these two
# learner are plotted.

ggplot(data = benchmark_results[benchmark_results$learner.id %in% c('randomForest', 'deeplearning'), ], 
       mapping = aes(x = mse.test.mean, y = learner.id, color = task.id)) + 
  geom_point() + 
  scale_color_brewer(palette = 'Dark2')

ggplot(data = benchmark_results[benchmark_results$learner.id %in% c('randomForest', 'deeplearning'), ], 
       mapping = aes(x = timepredict.test.mean, y = learner.id, color = task.id)) + 
  geom_point() + 
  scale_color_brewer(palette = 'Dark2')

# The feature selection does not seem to enhance performance measure. Based on
# the time needed to train the selected couples are then
# randomForest/trips_imp_fs and deeplearning/trips_imp_scale. 

# save benchmark results and clean session
saveRDS(benchmark_results, 'data/models/benchmark_results.rds')
rm(list = ls(pattern = 'benchmark'))
