fortunes::fortune(10)

?Map

library(parallelMap)
library(microbenchmark)

# adapted from: https://mllg.github.io/batchtools/articles/batchtools.html#example-1-approximation-of-pi

approximate_pi <- function(n) {
  nums <- matrix(runif(2 * n), ncol = 2)
  d <- sqrt(nums[, 1]^2 + nums[, 2]^2)
  4 * mean(d <= 1)
}
RNGkind("L'Ecuyer-CMRG")
set.seed(1410)

approximate_pi(5)

lapply(rep(1e5, 100), approximate_pi)

# under the hood parallel::mclapply
# również parallelStartMPI
parallelStartMulticore(4, show.info = TRUE)
parallelLapply(rep(1e5, 100), approximate_pi)
parallelStop()

parallelStartMulticore(4, show.info = FALSE)
microbenchmark(lapply = lapply(rep(1e5, 100), approximate_pi), 
               parallelLapply = parallelLapply(rep(1e5, 100), approximate_pi), 
               times = 1)
parallelStop()


# batchtools ----------------------
library(batchtools)

# ułatwienie: btlapply

registry <- makeRegistry(file.dir = "./file_registry", seed = 15390)
getDefaultRegistry()
# setDefaultRegistry()
batchMap(fun = approximate_pi, n = rep(1e5, 10))
getJobTable()
submitJobs(resources = list(walltime = 3600, memory = 1024))
getStatus()

loadResult(1)
