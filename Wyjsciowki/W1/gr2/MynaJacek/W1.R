constructor_lm <- function(..., comment=NULL) {
  model <- lm(...)
  create_time <- Sys.time()
  elapsed_time <- system.time(model)[3]
  structure(list(model = model, create_time = create_time, elapsed_time = elapsed_time, comment = comment, class = "my_class"))
}

summary.my_class <- function(x, ...) {
  print(summary(x[model]))
  print(x[comment])
  print(x[create_time])
  print(x[elapsed_time])
}

validator <- function(x, ...) {
  if(all("create_time", "elapsed_time", "comment", "model") %in% names(x)){
    TRUE
  }
  else{
    FALSE
  }
}

ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
my_object <- structure(lm(weight ~ group), class = "my_class")
