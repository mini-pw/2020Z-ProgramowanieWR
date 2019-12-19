library(mlr)

set.seed()

task_iris <- makeClassifTask(id = "Pierwsze spotkanie z mlr", data = iris, target = "Species")

lrn <- makeLearner("classif.randomForest", predict.type = "prob")

simple_bench <- benchmark(lrn, task_iris, measures = multiclass.aunp, 
          resamplings = makeResampleDesc(method = "CV", iters = 3))

lrn_tune <- makeTuneWrapper(learner = lrn, 
                resampling = makeResampleDesc(method = "CV", iters = 3), 
                par.set = makeParamSet(makeIntegerParam("ntree", lower = 500, upper = 1000)),
                control = makeTuneControlRandom(maxit = 50), 
                measures = multiclass.aunp)


res <- train(lrn_tune, task_iris)
print(getTuneResult(res))
