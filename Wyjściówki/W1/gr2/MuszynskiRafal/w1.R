new.lm_wrapper <- function(comment=NA, ...){
  model_date <- Sys.Date()
  start_time <- Sys.time()
  model <- lm(...)
  model_creation_time <- Sys.time() - start_time
  object <- list(model=model, model_creation_time=model_creation_time, model_date=model_date, comment=comment)
  class(object) <- "lm_wrapper"
  object
}

summary.lm_wrapper <- function(x, ...){
  print(summary(x$model))
  cat("model date:\n")
  print(x$model_date)
  cat("model creation time:\n")
  print(x$model_creation_time)
  cat("comment")
  print(x$comment)
}

is.lm_wrapper <- function(x, ...){
  class(x) == "lm_wrapper"
}

# test
lm(Sepal.Length ~ Sepal.Width, iris)

my_lm_wrapper <- new.lm_wrapper(Sepal.Length ~ Sepal.Width, iris)
summary(my_lm_wrapper)

is.lm_wrapper(my_lm_wrapper)
