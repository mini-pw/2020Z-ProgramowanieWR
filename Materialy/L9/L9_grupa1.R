library(mlr)
library(titanic)
library(ggplot2)

#set.seed()
set.seed(1, kind = "L'Ecuyer-CMRG")
configureMlr(show.info = TRUE)

# makeLearner ------------------------

listLearners()

rf <- makeLearner("classif.ranger", id = "ranger", predict.type = "prob")
rf1000 <- makeLearner("classif.ranger", id = "ranger1000", par.vals = list(num.trees = 1000), 
                      predict.type = "prob")
svm <- makeLearner("classif.ksvm", predict.type = "prob")

# listLearnerProperties

# makeClassifTask -----------------------------

library(dplyr)

mlr_friendly_titanic <- mutate(titanic_train, Survived = factor(Survived),
                               Pclass = factor(Pclass),
                               Sex = factor(Sex)) %>% 
  select(Survived, Pclass, Sex, Fare, Age) %>% 
  filter(!is.na(Age))

make_weights <- function(x) {
  x1 <- sum(x == x[1])
  ifelse(x == x[1], 1 - x1/length(x), x1/length(x))
}

titanic_task <- makeClassifTask(id = "titanic", data = mlr_friendly_titanic, target = "Survived")
titanic_task_weight <- makeClassifTask(id = "titanic_weight", data = mlr_friendly_titanic, 
                                       target = "Survived", 
                                       weights = make_weights(mlr_friendly_titanic[["Survived"]]))
# makeOversampleWrapper
# makeWeightedClassesWrapper

# makeResampleDesc ------------------------------------------------------

cv5 <- makeResampleDesc(method = "CV", iters = 5)
cv6 <- makeResampleDesc(method = "CV", iters = 6)

# benchmark

bench_res <- benchmark(learners = list(rf, rf1000),
                       tasks = list(titanic_task, titanic_task_weight), 
                       resamplings = list(cv5, cv6),
                       measures = auc, models = TRUE)
# listMeasures()
getBMRPerformances(bench_res)
getBMRAggrPerformances(bench_res)
getBMRPredictions(bench_res)
getBMRModels(bench_res)
plotBMRBoxplots(bench_res, pretty.names = FALSE) +
  scale_y_continuous(limits = c(0.5, 1))
plotBMRBoxplots(bench_res, pretty.names = FALSE, style = "violin")

# nested cross-validation ----

outer <- makeResampleDesc(method = "CV", iters = 3)
inner <- makeResampleDesc(method = "CV", iters = 5)

ranger_wrapper <- makeTuneWrapper(learner = makeLearner("classif.ranger", predict.type = "prob"), 
                                  resampling = inner, 
                                  par.set = makeParamSet(makeIntegerParam(id = "num.trees", lower = 50, upper = 1000)),
                                  control = makeTuneControlGrid(resolution = 5))

bench <- benchmark(ranger_wrapper, titanic_task, resamplings = outer, measures = auc)
rsp <- resample(learner = ranger_wrapper, task = titanic_task, resampling = outer, measures = auc, 
         extract = getTuneResult)

res <- getNestedTuneResultsOptPathDf(rsp)
res
