v <- c(P1 = 5,
       P2 = 4,
       P3 = 3,
       P4 = 2,
       P5 = 5,
       P6 = 4,
       P7 = 3,
       P8 = 4,
       P9 = 4,
       P10 = 3)


validateStudent <- function(student) {
  grades <- attr(student, 'grades')
  studentId <- attr(student, 'id')
  if (!is.numeric(studentId) | studentId < 0) {
    cat('Niepoprawny ID studenta')
    return(FALSE)
  } else if (!is.vector(grades) & !any(is.na(grades))) { 
    cat('Niepoprawny wektor ocen studenta')
    return(FALSE)
  } else {
    return(TRUE)
  }
  
  TRUE
}

createStudent <- function(name, studentId, grades) {
  
  student <- structure(name,
                       class='student', 
                       'id'=studentId,
                       'grades'=grades)
  
  if (!validateStudent(student)) {
    stop('Dane studenta zostaly niepoprawnie wprowadzone')
  }
  
  student
}

mean.student <- function(x, ...) {
  cat("Srednia ocen studenta to:\n")
  mean(attr(x, 'grades'))
}

student <- createStudent('Mateusz Polakowski', 1, v)
mean(student)
