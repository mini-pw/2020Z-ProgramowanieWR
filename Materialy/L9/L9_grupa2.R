library(mlr)
library(titanic)
library(dplyr)

# makeLearner --------------------

lrn_ranger <- makeLearner("classif.ranger", predict.type = "prob", id = "ranger")
lrn_ranger420 <- makeLearner("classif.ranger", predict.type = "prob", 
                             par.vals = list(num.trees = 420), id = "ranger420")

# getHyperPars(lrn_ranger420)

mlr_friendly_titanic <- mutate(titanic_train, Survived = factor(Survived),
                               Pclass = factor(Pclass),
                               Sex = factor(Sex)) %>% 
  select(Survived, Pclass, Sex, Age, Fare) %>% 
  na.omit

titanic_task <- makeClassifTask(id = "titanic task", data = mlr_friendly_titanic, 
                                target = "Survived")

compute_weights <- function(x) {
  x1 <- sum(x == x[1])
  x2 <- sum(x != x[1])
  ifelse(x == x[1], x2/length(x), x1/length(x))
}

titanic_task_weighted <- makeClassifTask(id = "titanic task weighted", data = mlr_friendly_titanic, 
                                         target = "Survived", 
                                         weights = compute_weights(mlr_friendly_titanic[["Survived"]]))

# makeOversampleWrapper()
# makeWeightedClassesWrapper()

# resampling -------

cv5 <- makeResampleDesc(method = "CV", iters = 5)
cv7_stratified <- makeResampleDesc(method = "CV", iters = 7, stratify = TRUE)
cv7 <- makeResampleDesc(method = "CV", iters = 7)

# benchmark --------

bench_res <- lapply(list(cv5 = cv5, cv7s = cv7_stratified, cv7 = cv7), function(ith_resampling) 
  benchmark(learners = list(lrn_ranger, lrn_ranger420),
            tasks = list(titanic_task, titanic_task_weighted),
            resamplings = ith_resampling, measures = auc)
)

getBMRAggrPerformances(bench_res[[1]])
plotBMRBoxplots(bench_res[[1]], pretty.names = FALSE)

bench_res_df <- lapply(names(bench_res), function(k)
  lapply(names(getBMRPerformances(bench_res[[k]])), function(j) 
    lapply(names(getBMRPerformances(bench_res[[k]])[[j]]), function(i)
      data.frame(resampling = k, task = j, model = i, getBMRPerformances(bench_res[[k]])[[j]][[i]],
                 stringsAsFactors = FALSE)) %>% 
      bind_rows()
  ) %>% 
    bind_rows()
) %>% 
  bind_rows()

library(ggplot2)

ggplot(bench_res_df, aes(x = model, y = auc, color = resampling)) +
  geom_boxplot() +
  facet_wrap(~ task) +
  theme_bw(base_size = 18)

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

  