
# The hyperparameters of the randonForest are tuned. The dataset used is the one
# with imputed data and feature selection trips_imp_fs. Different combination of
# hyperparamters are tested and the one resulting in the best performance is
# retained.

# Inputs: the task trips_imp_fs, the train and test sets. 

# Outputs: the values of the hyperparameters leading to the best performance.

# Packages ----------------------------------------------------------------

library('mlr') # to tune the learner
library('parallelMap') # parallel computing


# Initiate tuning ---------------------------------------------------------

# The different objects needed to perform the tuning are imported. A parallel
# backend is instantiate to speed up the process.

# Import the task
task <- readRDS('data/tasks/task_imp_fs.rds')

# Base learner
learner <- makeLearner('regr.h2o.randomForest', id = 'rf')

# list of all hyperparameters of the algorithm
learner$par.set

# Set up parallel backend.
parallelStartSocket(cpus = 2)
