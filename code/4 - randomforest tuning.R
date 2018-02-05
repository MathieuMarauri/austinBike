
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

# import the task
task <- readRDS('data/tasks/task_imp_fs.rds')

# base learner
learner <- makeLearner('regr.h2o.randomForest', id = 'rf')

# list of all hyperparameters of the algorithm
learner$par.set

# set up parallel backend.
parallelStartSocket(cpus = 2)


# RandomForest tuning -----------------------------------------------------

# The possible values for some hyperparameters are defined here. A optimization
# algorithm is also defined along with an evaluation method. After the tuning
# the best combination of parameters values is selected to build the final
# RandomForest learner.

# parameter space
parameter_space <- makeParamSet(
  makeDiscreteParam(id = "ntrees", values = c(50, 100, 150, 200)),
  makeDiscreteParam(id = 'max_depth', values = c(16, 18, 20, 22, 24)),
  makeDiscreteParam(id = 'nbins', values = c(10, 15, 20, 25, 30)),
  makeNumericParam(id = 'mtries', values = c(2, 4, 6, 8, 10))
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

# stop the parallelization
parallelStop()
