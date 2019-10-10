

student <- function(id,grades, ...) {
  # ... for future reference
  if (!is.character(id)) stop("Id must be string")
  if (!is.list(grades)) stop("Grades must be named list")
  for (i in 1:10) {
    p_name <- paste0('P', i)
    if (!(p_name %in% names(grades))) {
      stop(paste0("Grade for ", p_name, " is missing"))
    }
    if (length(grades) != 10) {
      stop("Grades for unknown P were given")
    }
  }
  value <- list(id=id, grades=grades)
  
  attr(value, "class") <- "student"
  value
}

s <- student("42", c(P1=5,P2=4,P3=5,P4=4,P5=4,P6=5,P7=5,P8=4,P9=5,P10=4))

s <- student(42, c(P1=5,P2=4,P3=5,P4=4,P5=4,P6=5,P7=5,P8=4,P9=5,P10=4))
s <- student("42", c(P1=5,P2=4,P3=5,P4=4,P5=4,P6=5,P7=5,P8=4,P9=5,P10=4, P22=4))
s <- student("42", c(P2=4,P3=5,P4=4,P5=4,P6=5,P7=5,P8=4,P9=5,P10=4))
s <- student("42", "xd")
s <- student("42", "xd", "xd")


mean.student <- function(x, ...) {
  mean(x$grades)
}

mean(s)
