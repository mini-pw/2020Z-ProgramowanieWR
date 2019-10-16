# Konstruktor
constructor <- function(student_id, grades) {
  student <- structure(list(student_id = student_id,
                       grades = c(p1 = grades[1], p2 = grades[2], p3 = grades[3], p4 = grades[4], p5 = grades[5], p6 = grades[6], p7 = grades[7], p8 = grades[8], p9 = grades[9], p10 = grades[10])),
                       class = "student")
  student
}

# Walidator
validator <- function(object) {
  if(class(object) == "student" && 
     class(object$student_id) == "character" && length(object$student_id) == 1 &&
     length(object$grades) == 10 && all(object$grades >= 2) && all(object$grades <= 5)) {
    TRUE
  }
  else {
    FALSE
  }
}

# Srednia ocen
mean.student <- function(x, ...) {
  mean(x$grades)
}

# Stworzenie obiektu
student_object <- constructor("123", c(3, 5, 3, 4, 4, 5, 3, 5, 3, 4))
student_object

validator(student_object)

mean(student_object)
 