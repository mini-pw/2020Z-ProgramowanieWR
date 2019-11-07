x <- 1:100
y <- x + runif(length(x), 0,30)
  
lm_wrap_constructor <- function(x,y,...){
  start_time <- Sys.time()
  model <- lm(x~y,...)
  end_time <- Sys.time()
  
  time_taken = end_time - start_time
  x <- list(model, time_taken)
  class(x) <- 'lm_wrap'
  return(x)
}

lm_wrap <- function(x,y,...){
  lm_wrap_class <- lm_wrap_constructor(x,y)
  class(lm_wrap_class)
  attr(lm_wrap_class, "model") <- lm_wrap_class[1]
  attr(lm_wrap_class, "time") <- lm_wrap_class[2]
  attr(lm_wrap_class, "comment") <- ""
  return(lm_wrap_class)
}
attributes(lm_wrap(x,y))
test <- lm_wrap(x,y)

summary.lm_wrap <- function(object){
  model_attr <- attr(object, "model")
  print(model_attr)
  paste(attr(object, "time"))
  paste(attr(object, "comment"))
}

summary(test)

