library(kernlab)
library(mlr)
library(titanic)
library(ggplot2)

data("spam")

set.seed(1)

rf <- makeLearner("classif.ranger", id = "ranger", predict.type = "prob")

rf1000 <- makeLearner("classif.ranger", id = "ranger1000", par.vals = list(num.trees = 1000, min.node.size = 20), 
                      predict.type = "prob")

rf1000_10 <- makeLearner("classif.ranger", id = "ranger1000_10", par.vals = list(num.trees = 1000, min.node.size = 10), 
                      predict.type = "prob")

rf1000_5 <- makeLearner("classif.ranger", id = "ranger1000_5", par.vals = list(num.trees = 1000, min.node.size = 5), 
                         predict.type = "prob")

xgb_learner <- makeLearner(
  "classif.xgboost",
  predict.type = "prob",
  par.vals = list(
    objective = "binary:logistic",
    eval_metric = "error",
    nrounds = 200,
    max_depth = 7
  )
)

my_task_01 <- makeClassifTask(id = "spam_01", data = spam, target = "type")

cv6 <- makeResampleDesc(method = "CV", iters = 6)

bench_res <- benchmark(learners = list(rf1000_5, rf1000_10),
                       tasks = list(my_task_01), 
                       resamplings = list(cv6),
                       measures = auc, models = TRUE)


## BEST - AUC = 0.9881249
bench_res_xgboost <- benchmark(learners = list(xgb_learner),
                       tasks = list(my_task_01), 
                       resamplings = list(cv6),
                       measures = auc, models = TRUE)

