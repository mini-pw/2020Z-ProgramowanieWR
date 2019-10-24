constructor_my_lm <- function(formula, data, comment = NULL, ...) {
  if (is.null(data)){
    print("Data is NULL")
    return()
  }
  time <- system.time(m <- lm(formula, data))
  x <- list(model = m, comment = comment, time = time[[3]], date = Sys.Date())
  class(x) <- "my_lm"
  return(x)
}

summary.my_lm <- function(x, ...) {
  print(summary(x$model))
  cat("Comment:\n")
  print(x$comment)
  cat("Time:\n")
  print(x$time)
  cat("Date:\n")
  print(x$date)
}

df <- data.frame(x = 1:5, y = 6:10)
m <- constructor_my_lm(x ~ y, df, 'This is comment')

summary(m)
