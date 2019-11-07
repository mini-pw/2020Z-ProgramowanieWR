
library(MASS)


constructor <- function(..., komentarz = NULL){
    model <- lm(...)
    time_xtra <- system.time(lm(...))
    date_xtra <- Sys.Date()
    ret <- list(model = model, time_xtra = time_xtra, date_xtra = date_xtra,komentarz=komentarz)
    class(ret) <- 'lm_extra'
    ret    
}
test <- constructor(time~dist,data=hills)

summary.lm_extra <- function(object, ...){
  print(summary(object$model))
  cat('Czas:\n')
  cat(object$time_xtra)
  cat('\n')
  cat('Data:\n')
  print(object$date_xtra)
  cat('\n')
  cat('Komentarz:\n')
  cat(object$komentarz)
  cat('\n')
}
summary(test)

validator <- function(lm_extra){
  # if () stop ("text")
  return(TRUE)
}
