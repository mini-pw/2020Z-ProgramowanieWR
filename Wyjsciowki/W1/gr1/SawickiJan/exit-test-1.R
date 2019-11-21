construct_student = function(id, grades) {
  new_student <-
    structure(list(
      id = id,
      grades = c(
        p1 = grades[1],
        p2 = grades[2],
        p3 = grades[3],
        p4 = grades[4],
        p5 = grades[5],
        p6 = grades[6],
        p7 = grades[7],
        p8 = grades[8],
        p9 = grades[9],
        p10 = grades[10]
      )
    ),
    class = "student")
  new_student
}

student = construct_student("000000", c(3, 3, 3, 4, 3, 3, 3, 3, 3, 3))

student$grades

mean.student = function(x, ...) {
  mean(x$grades)
}

mean(student)

validate_student = function(object) {
  if (class(object) == "student" &&
      is.character(object$id) && 
      is.numeric(object$grades) && 
      length(object$grades) == 10 && 
      all(object$grades >=2) &&
      all(object$grades <= 5)
      ) {
    TRUE
  }
  else{
    FALSE
  }
}

validate_student(construct_student("000000", c(3, 3, 3, 4, 3, 3, 3, 3, 3, 3))) == TRUE
validate_student(data.frame()) == FALSE
validate_student(construct_student(123, c(3, 3, 3, 4, 3, 3, 3, 3, 3, 3))) == FALSE
validate_student(construct_student("000000", c(0, 3, 3, 4, 3, 3, 3, 3, 3, 3))) == FALSE