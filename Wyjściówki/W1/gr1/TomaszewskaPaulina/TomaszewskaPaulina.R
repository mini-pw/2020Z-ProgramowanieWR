

# a constructor function for the "student" class
student <- function(id, oceny) {
  # we can add our own integrity checks
  if (!is.character(id))  stop("it's not an ID")
  value <- mean(oceny)
  for (i in 1:length(oceny)){
    if(oceny[i]>6 || oceny[i]<1)  stop("grades must be between 1-6")
  }
  if (is.na(oceny)) stop("Some grades are missing")
  # class can be set using class() or attr() function
  attr(value, "class") <- "student"
  value
}


mean.student<-function(x,...){
  mean(x)
}

oceny=c(P1=5, P2=4, P3=2, P4=6, P5=4, P6=3, P7=4, P8=5, P9=3, P10=4)

s <- list(id=1234, oceny)

class(s) <- "student"
