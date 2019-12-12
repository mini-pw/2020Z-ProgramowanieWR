library(microbenchmark)
library(dplyr)
library('randomForest')


my_plan<-drake_plan(
  data=iris,
  processed_data=data %>% mutate(Species, Species=ifelse(Species=='setosa', 'setosa', 'not-setosa')),
  task = makeClassifTask(id = "drake_test1", data = processed_data, target = "Species"),
  bench1 = benchmark(learners =makeLearners(c("classif.randomForest","classif.ksvm", "classif.nnet")), tasks = task),
  preds1 = data.frame(getBMRAggrPerformances(bench1)),
  boxplot(preds1),
  write.csv(processed_data, "input_data.csv")
)


make(my_plan)
