library(future)
library(mlr)

head(iris)
task <- makeClassifTask(id = "1", data = iris, target = "Species")
rdesc <- makeResampleDesc("CV", iters = 50)
plan(multisession)
a %<-% {
  mlr::benchmark(learners = list(makeLearner("classif.randomForest")), tasks = task, resamplings = rdesc)
}
mlr::benchmark(learners = list(makeLearner("classif.ranger")), tasks = task, resamplings = rdesc)
a
