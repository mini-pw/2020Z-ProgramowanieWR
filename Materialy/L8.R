library(archivist)

# tylko do celów demonstracyjnych
cacheRepo <- tempfile()
createLocalRepo(cacheRepo)
# https://github.com/ropensci/git2r

library(ggplot2)
# objects of class ggplot for which the session_info was archvied
# https://github.com/pbiecek/graphGallery

md5plots <- searchInRemoteRepo(
  pattern = c("class:ggplot", "session_info"), 
  intersect = TRUE, repo = "graphGallery", 
  user = "pbiecek", fixed = FALSE
)

plots <- lapply(md5plots, function(pl) {
  loadFromRemoteRepo(
    md5hash = pl, 
    repo = "graphGallery",
    user = "pbiecek",
    value = TRUE
  ) + 
    ggtitle(pl)
})

aread('pbiecek/graphGallery/5e9558aed86ab3d6657f52441d0f9b5a')

library(drake)
library(mlr)
library(kernlab)
dat <- read.csv("https://raw.githubusercontent.com/mini-pw/2020Z-ProgramowanieWR/master/Wyjsciowki/W2/gr1/SawickiJan/ShinyIris/iris.csv")
task <- makeClassifTask(id = "drake_test", data = dat, target = "variety")
bench <- benchmark(learners = makeLearner("classif.ksvm"), tasks = task)
preds <- data.frame(getBMRPredictions(bench))

# ls()
# mean(x <- 1L:5)
# ls()

my_first_plan <- drake_plan(
  dat = read.csv("https://raw.githubusercontent.com/mini-pw/2020Z-ProgramowanieWR/master/Wyjsciowki/W2/gr1/SawickiJan/ShinyIris/iris.csv"),
  task = makeClassifTask(id = "drake_test", data = dat, target = "variety"),
  bench = benchmark(learners = makeLearner("classif.randomForest"), tasks = task),
  preds = data.frame(getBMRPredictions(bench))
)

make(my_first_plan)
readd("bench")

