library(mlr)
library(kernlab)
library(dplyr)

# makeLearner --------------------
data(ticdata)
smp_size <- floor(0.75 * nrow(ticdata))
set.seed(123)

#train_ind <- sample(seq_len(nrow(ticdata)), smp_size)
#ticdata_train <- mlr_friendly_ticdata [train_ind,]
#ticdata_test <- mlr_friendly_ticdata [-train_ind,]
ticdata_with_dummies <- createDummyFeatures(ticdata) %>% mutate(CARAVAN.insurance = factor(CARAVAN.insurance)) %>% 
  select(-CARAVAN.noinsurance) 

mlr_friendly_ticdata <- vapply(ticdata_with_dummies, function(x) length(unique(x)) > 1, logical(1L))


lrn_ranger <- makeLearner("classif.ranger", predict.type = "prob", id = "ranger")
lrn_xgb <- makeLearner("classif.xgboost",
                       predict.type = "prob",
                       par.vals = list(
                         objective = "binary:logistic",
                         eval_metric = "error",
                         nrounds = 100
                       ), id = "xgb")
#lrn_lda<- makeLearner("classif.lda", predict.type = "prob", 
#                             par.vals = list(nu = 10), id = "lda")

getHyperPars(lrn_xgb)


ticdata_task <- makeClassifTask(id = "titanic task", data = mlr_friendly_ticdata, 
                                target = "CARAVAN.insurance")

compute_weights <- function(x) {
  x1 <- sum(x == x[1])
  x2 <- sum(x != x[1])
  ifelse(x == x[1], x2/length(x), x1/length(x))
}

ticdata_task_weighted <- makeClassifTask(id = "ticdata task weighted", data = mlr_friendly_ticdata, 
                                         target = "CARAVAN.insurance", 
                                         weights = compute_weights(mlr_friendly_ticdata[["CARAVAN.insurance"]]))
# makeOversampleWrapper()
# makeWeightedClassesWrapper()

# resampling -------

#cv5 <- makeResampleDesc(method = "CV", iters = 5)
#cv7_stratified <- makeResampleDesc(method = "CV", iters = 7, stratify = TRUE)
cv7 <- makeResampleDesc(method = "CV", iters = 3)

# benchmark --------

bench_res <- lapply(list(cv7 = cv7), function(ith_resampling) 
  benchmark(learners = list(lrn_ranger, lrn_xgb),
            tasks = list(ticdata_task, ticdata_task_weighted),
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
                                  control = makeTuneControlGrid(resolution = 15))

bench <- benchmark(ranger_wrapper, titanic_task, resamplings = outer, measures = auc)
rsp <- resample(learner = ranger_wrapper, task = titanic_task, resampling = outer, measures = auc, 
                extract = getTuneResult)

  