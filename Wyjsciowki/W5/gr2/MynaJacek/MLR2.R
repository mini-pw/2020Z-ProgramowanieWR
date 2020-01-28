library(kernlab)
library(mlr)


data(ticdata)
set.seed(999)

task_ticdata <- makeClassifTask(data = ticdata, target = "CARAVAN")

rangerLearner <- makeLearner("classif.ranger", predict.type = "prob")

penalizedLearner <- makeLearner("classif.penalized", predict.type = "prob", par.vals = list(lambda1 = 1, lambda2 = 2))

learners <- list(penalizedLearner, rangerLearner)

benchmark_results <- benchmark(
  learners,
  task_ticdata,
  measures = auc,
  resamplings = cv5
)

outer <- makeResampleDesc(method = "CV", iters = 3)
inner <- makeResampleDesc(method = "CV", iters = 5)

ranger_wrapper <- makeTuneWrapper(learner = makeLearner("classif.ranger", predict.type = "prob"), 
                                  resampling = inner, 
                                  par.set = makeParamSet(makeIntegerParam(id = "num.trees", lower = 50, upper = 1000)),
                                  control = makeTuneControlGrid(resolution = 5))

bench <- benchmark(ranger_wrapper, task_ticdata, resamplings = outer, measures = auc)

