



student <- function(id,grades) {
  # ... for future reference
  if (!validate_id(id)) stop("Id must be string")
  if (!validate_grades(grades)) stop("Invalid grades")
  value <- list(id=id, grades=grades)
  attr(value, "class") <- "student"
  value
}

validate_id <- function(id) {
  return(is.character(id))
}

validate_grades <- function(grades) {
  for (i in 1:10) {
    p_name <- paste0('P', i)
    if (!(p_name %in% names(grades))) {
      return(FALSE);
    }
    if (length(grades) != 10) {
      return(FALSE);
    }
  }
  return(TRUE)
}

validator <- function(student) {
  if (!validate_id(student$id)) {
    return(FALSE);
  }
  if (!validate_grades(student$grades)) {
    return(FALSE);
  }
  return(TRUE);
}

s <- student("42", c(P1=5,P2=4,P3=5,P4=4,P5=4,P6=5,P7=5,P8=4,P9=5,P10=4))

s <- student(42, c(P1=5,P2=4,P3=5,P4=4,P5=4,P6=5,P7=5,P8=4,P9=5,P10=4))
s <- student("42", c(P1=5,P2=4,P3=5,P4=4,P5=4,P6=5,P7=5,P8=4,P9=5,P10=4, P22=4))
s <- student("42", c(P2=4,P3=5,P4=4,P5=4,P6=5,P7=5,P8=4,P9=5,P10=4))
s <- student("42", c(P1=5,P2=4,P3=5,P4=4,P5=4,P6=5,P7=5,P8=4,P9=5,P10=NA))
s <- student("42", "xd")
s <- student("42", "xd", "xd")
s$grades
validator(s)

mean.student <- function(x, ...) {
  mean(x$grades, na.rm=TRUE)
}

mean(s)
